from flask import Flask, render_template_string, jsonify, request
import numpy as np
import random

app = Flask(__name__)

# Configuración del mundo
TAMANIO_MUNDO = 50

# Estados de cada celda
VACIO = 0
PASTO = 1
PRESA = 2
DEPREDADOR = 3

class Celda:
    def __init__(self):
        self.pasto = False
        self.animal = None  # None o dict con keys 'tipo' (PRESA o DEPREDADOR), 'energia' (int)

class Mundo:
    def __init__(self, tamaño, prop_pasto, prop_presas, prop_depredadores):
        self.tamaño = tamaño
        self.grilla = [[Celda() for _ in range(tamaño)] for _ in range(tamaño)]
        self.inicializar_mundo(prop_pasto, prop_presas, prop_depredadores)

        # Historial para almacenar datos por iteración
        self.historial = []
        # Conteos previos para calcular nacimientos y muertes
        self.conteo_presas_anterior = 0
        self.conteo_depredadores_anterior = 0
        self.conteo_pasto_anterior = 0

    def inicializar_mundo(self, prop_pasto, prop_presas, prop_depredadores):
        for i in range(self.tamaño):
            for j in range(self.tamaño):
                if random.random() < prop_pasto:
                    self.grilla[i][j].pasto = True
                if random.random() < prop_presas:
                    self.grilla[i][j].animal = {'tipo': PRESA, 'energia': 6}  # energía inicial
                elif random.random() < prop_depredadores:
                    self.grilla[i][j].animal = {'tipo': DEPREDADOR, 'energia': 20}  # energía inicial

        # Inicializar conteos previos para el primer paso
        pasto_cuenta, presa_cuenta, depredador_cuenta = self.contar()
        self.conteo_presas_anterior = presa_cuenta
        self.conteo_depredadores_anterior = depredador_cuenta
        self.conteo_pasto_anterior = pasto_cuenta
        # Guardar estado inicial en historial
        self.historial = [{
            'iteracion': 0,
            'pasto': pasto_cuenta,
            'presas': presa_cuenta,
            'depredadores': depredador_cuenta,
            'nacimientos_presas': 0,
            'muertes_presas': 0,
            'nacimientos_depredadores': 0,
            'muertes_depredadores': 0,
        }]

    def vecinos(self, x, y):
        vecinos = []
        for dx in [-1, 0, 1]:
            for dy in [-1, 0, 1]:
                if dx == 0 and dy == 0:
                    continue
                nx, ny = (x + dx) % self.tamaño, (y + dy) % self.tamaño
                vecinos.append((nx, ny))
        return vecinos

    def paso(self):
        nueva_grilla = [[Celda() for _ in range(self.tamaño)] for _ in range(self.tamaño)]
        # Regeneración de pasto
        for i in range(self.tamaño):
            for j in range(self.tamaño):
                if (not self.grilla[i][j].pasto) and (self.grilla[i][j].animal is None):
                    if random.random() < 0.02:
                        nueva_grilla[i][j].pasto = True
                else:
                    nueva_grilla[i][j].pasto = self.grilla[i][j].pasto

        movido = np.zeros((self.tamaño, self.tamaño), dtype=bool)

        coords = [(i, j) for i in range(self.tamaño) for j in range(self.tamaño)]
        random.shuffle(coords)

        # Procesar depredadores
        for (x, y) in coords:
            celda = self.grilla[x][y]
            if celda.animal and celda.animal['tipo'] == DEPREDADOR and not movido[x][y]:
                depredador = celda.animal.copy()
                depredador['energia'] -= 2 # Pierde energía al moverse
                if depredador['energia'] <= 0:
                    continue

                vecinos = self.vecinos(x, y)
                random.shuffle(vecinos)
                lugares_presa = [(nx, ny) for (nx, ny) in vecinos if self.grilla[nx][ny].animal and self.grilla[nx][ny].animal['tipo'] == PRESA]
                target_x, target_y = x, y

                if lugares_presa:
                    target_x, target_y = lugares_presa[0]
                    depredador['energia'] += 20  # Gana energía al comer
                else:
                    lugares_vacios = [(nx, ny) for (nx, ny) in vecinos if self.grilla[nx][ny].animal is None]
                    if lugares_vacios:
                        target_x, target_y = lugares_vacios[0]

                if (target_x, target_y) != (x, y) and not movido[target_x][target_y]:
                    nueva_grilla[target_x][target_y].animal = depredador
                    nueva_grilla[target_x][target_y].pasto = self.grilla[target_x][target_y].pasto
                    movido[target_x][target_y] = True
                else:
                    if not movido[x][y]:
                        nueva_grilla[x][y].animal = depredador
                        nueva_grilla[x][y].pasto = self.grilla[x][y].pasto
                        movido[x][y] = True

        # Procesar presas
        for (x, y) in coords:
            if movido[x][y]:
                continue
            celda = self.grilla[x][y]
            if celda.animal and celda.animal['tipo'] == PRESA:
                presa = celda.animal.copy()
                presa['energia'] -= 1
                if presa['energia'] <= 0:
                    continue

                vecinos = self.vecinos(x, y)
                random.shuffle(vecinos)

                lugares_pasto = [(nx, ny) for (nx, ny) in vecinos if self.grilla[nx][ny].animal is None and self.grilla[nx][ny].pasto]
                lugares_vacios = [(nx, ny) for (nx, ny) in vecinos if self.grilla[nx][ny].animal is None and not self.grilla[nx][ny].pasto]

                target_x, target_y = x, y
                if lugares_pasto:
                    target_x, target_y = lugares_pasto[0]
                    presa['energia'] += 4
                elif lugares_vacios:
                    target_x, target_y = lugares_vacios[0]

                if (target_x, target_y) != (x, y) and not movido[target_x][target_y]:
                    nueva_grilla[target_x][target_y].animal = presa
                    if lugares_pasto:
                        nueva_grilla[target_x][target_y].pasto = False
                    else:
                        nueva_grilla[target_x][target_y].pasto = self.grilla[target_x][target_y].pasto
                    movido[target_x][target_y] = True
                else:
                    if not movido[x][y]:
                        nueva_grilla[x][y].animal = presa
                        nueva_grilla[x][y].pasto = self.grilla[x][y].pasto
                        movido[x][y] = True

        self.grilla = nueva_grilla

        pasto_cuenta, presa_cuenta, depredador_cuenta = self.contar()

        nacimientos_presas = max(0, presa_cuenta - self.conteo_presas_anterior)
        muertes_presas = max(0, self.conteo_presas_anterior - presa_cuenta)
        nacimientos_depredadores = max(0, depredador_cuenta - self.conteo_depredadores_anterior)
        muertes_depredadores = max(0, self.conteo_depredadores_anterior - depredador_cuenta)

        self.conteo_presas_anterior = presa_cuenta
        self.conteo_depredadores_anterior = depredador_cuenta
        self.conteo_pasto_anterior = pasto_cuenta

        iteracion_actual = len(self.historial)
        self.historial.append({
            'iteracion': iteracion_actual,
            'pasto': pasto_cuenta,
            'presas': presa_cuenta,
            'depredadores': depredador_cuenta,
            'nacimientos_presas': nacimientos_presas,
            'muertes_presas': muertes_presas,
            'nacimientos_depredadores': nacimientos_depredadores,
            'muertes_depredadores': muertes_depredadores,
        })

    def contar(self):
        pasto_cuenta = 0
        presa_cuenta = 0
        depredador_cuenta = 0
        for i in range(self.tamaño):
            for j in range(self.tamaño):
                celda = self.grilla[i][j]
                if celda.pasto:
                    pasto_cuenta += 1
                if celda.animal:
                    if celda.animal['tipo'] == PRESA:
                        presa_cuenta += 1
                    elif celda.animal['tipo'] == DEPREDADOR:
                        depredador_cuenta += 1
        return pasto_cuenta, presa_cuenta, depredador_cuenta

mundo = None
simulacion_corriendo = False

HTML_PAGE = """
<!DOCTYPE html>
<html>
<head>
<meta charset="utf-8" />
<title>Simulación Depredador-Presa </title>
<script src="https://cdn.plot.ly/plotly-latest.min.js"></script>

<style>
    body {
        font-family: 'Segoe UI', 'PT Sans', Verdana, sans-serif;
        margin: 40px;
        background-color: #f4f6f8;
        color: #2c3e50;
    }

    .container {
        max-width: 1200px;
        margin: auto;
        padding-top: 40px;
    }

    h1 {
        color: #2c3e50;
        text-align: center;
        margin-bottom: 30px;
        font-size: 2.5em;
    }

    .controls, .download-controls {
        display: flex;
        flex-wrap: wrap;
        gap: 20px;
        justify-content: center;
        align-items: center;
        margin-bottom: 20px;
        background-color: #ffffff;
        padding: 20px;
        border-radius: 12px;
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
    }

    .controls label, .download-controls label {
        margin-right: 8px;
        font-weight: 600;
    }

    .controls input {
        width: 100px;
        padding: 8px;
        font-size: 14px;
        border-radius: 8px;
        border: 1px solid #ccc;
        background-color: #fafafa;
        transition: border 0.3s ease;
    }

    .controls input:focus {
        border-color: #3498db;
        outline: none;
        background-color: #fff;
    }

    .buttons {
        text-align: center;
        margin-bottom: 20px;
    }

    button {
        font-size: 15px;
        padding: 10px 22px;
        margin: 5px;
        border: none;
        border-radius: 12px;
        background-color: #3498db;
        color: white;
        cursor: pointer;
        transition: all 0.3s ease;
        box-shadow: 0 4px 10px rgba(0, 0, 0, 0.1);
        display: inline-flex;
        align-items: center;
        gap: 8px;
    }

    button:disabled {
        background-color: #bdc3c7;
        color: #ecf0f1;
        cursor: not-allowed;
    }

    button:hover:not(:disabled) {
        background-color: #2980b9;
        transform: translateY(-2px);
        box-shadow: 0 6px 12px rgba(0, 0, 0, 0.15);
    }

    button svg {
        width: 16px;
        height: 16px;
        fill: white;
    }

    #status {
        text-align: center;
        margin: 15px 0;
        font-size: 18px;
        font-weight: bold;
        color: #e74c3c;
    }

    #graph, #statsGraph {
        width: 95vw;
        height: 60vh;
        margin: auto;
        margin-bottom: 60px;
        background-color: white;
        border-radius: 16px;
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.08);
        padding: 10px;
    }

    #finalResults {
        background: #ffffff;
        border-radius: 16px;
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.1);
        padding: 24px;
        max-width: 700px;
        margin: 40px auto;
        font-size: 16px;
        color: #2c3e50;
        line-height: 1.6;
    }

    /* Nuevo: Panel informativo */
    
</style>

</head>
<body>
<h1>Simulación Depredador-Presa</h1>

<div class="controls">
    <label for="propPasto">Proporción de Pasto:</label>
    <input type="number" id="propPasto" value="0.6" step="0.01" min="0" max="1" />

    <label for="propPresas">Proporción de Presas:</label>
    <input type="number" id="propPresas" value="0.15" step="0.01" min="0" max="1" />

    <label for="propDepredadores">Proporción de Depredadores:</label>
    <input type="number" id="propDepredadores" value="0.05" step="0.01" min="0" max="1" />
</div>

<div class="buttons">
    <button id="iniciarBtn">Iniciar Simulación</button>
    <button id="detenerBtn" disabled>Detener Simulación</button>
</div>

<div id="status">Estado: Detenida</div>
<div id="graph" style="width: 100%; height: 100%;"></div>
<div id="graph"></div>

<h2 style="text-align:center;">Tasas de Natalidad y Mortalidad</h2>
<div id="statsGraph"></div>

<div class="download-controls" style="justify-content:center;">
    <button id="descargarJsonBtn" disabled>Descargar Datos JSON</button>
    <button id="descargarCsvBtn" disabled>Descargar Datos CSV</button>
</div>

<div id="finalResults"></div>

<script>
let corriendo = false;
let intervaloId = null;

const graphDiv = document.getElementById('graph');
const statsGraphDiv = document.getElementById('statsGraph');
const statusDiv = document.getElementById('status');
const iniciarBtn = document.getElementById('iniciarBtn');
const detenerBtn = document.getElementById('detenerBtn');
const descargarJsonBtn = document.getElementById('descargarJsonBtn');
const descargarCsvBtn = document.getElementById('descargarCsvBtn');
const resultadosFinalesDiv = document.getElementById('finalResults');

let historialDatos = [];

const maxPuntos = 100;

// Gráficas poblaciones
let trazaPasto = {x: [], y: [], mode: 'lines+markers', name: 'Pasto', line: {color: 'green'}};
let trazaPresas = {x: [], y: [], mode: 'lines+markers', name: 'Presas', line: {color: 'blue'}};
let trazaDepredadores = {x: [], y: [], mode: 'lines+markers', name: 'Depredadores', line: {color: 'red'}};
let diseño = {
    title: 'Evolución de Poblaciones',
    xaxis: {title: 'Iteración'},
    yaxis: {title: 'Cantidad', rangemode: 'tozero'},
    legend: {orientation: 'h'}
};
Plotly.newPlot(graphDiv, [trazaPasto, trazaPresas, trazaDepredadores], diseño);

// Gráficas natalidad y mortalidad
let trazaNacimientosPresas = {x: [], y: [], mode: 'lines+markers', name: 'Nacimientos Presas', line: {color: '#007acc'}};
let trazaMuertesPresas = {x: [], y: [], mode: 'lines+markers', name: 'Muertes Presas', line: {color: '#005fa3'}};
let trazaNacimientosDepredadores = {x: [], y: [], mode: 'lines+markers', name: 'Nacimientos Depredadores', line: {color: '#ff6347'}};
let trazaMuertesDepredadores = {x: [], y: [], mode: 'lines+markers', name: 'Muertes Depredadores', line: {color: '#cc4c39'}};
let diseñoStats = {
    title: 'Tasas de Natalidad y Mortalidad',
    xaxis: {title: 'Iteración'},
    yaxis: {title: 'Cantidad', rangemode: 'tozero'},
    legend: {orientation: 'h'}
};
Plotly.newPlot(statsGraphDiv, [trazaNacimientosPresas, trazaMuertesPresas, trazaNacimientosDepredadores, trazaMuertesDepredadores], diseñoStats);

iniciarBtn.onclick = () => {
    if (!corriendo) {
        corriendo = true;
        iniciarBtn.disabled = true;
        detenerBtn.disabled = false;
        descargarJsonBtn.disabled = true;
        descargarCsvBtn.disabled = true;
        resultadosFinalesDiv.innerHTML = '';
        historialDatos = [];
        statusDiv.textContent = "Estado: Corriendo";
        const propPasto = parseFloat(document.getElementById('propPasto').value);
        const propPresas = parseFloat(document.getElementById('propPresas').value);
        const propDepredadores = parseFloat(document.getElementById('propDepredadores').value);
        fetch(`/reset?pasto=${propPasto}&presas=${propPresas}&depredadores=${propDepredadores}`).then(() => {
            reiniciarGraficas();
            pasoSim(0);
        });
    }
};

detenerBtn.onclick = () => {
    corriendo = false;
    iniciarBtn.disabled = false;
    detenerBtn.disabled = true;
    statusDiv.textContent = "Estado: Detenida";
    if (intervaloId) clearTimeout(intervaloId);
    if(historialDatos.length > 0){
        descargarJsonBtn.disabled = false;
        descargarCsvBtn.disabled = false;
        mostrarResultadosFinales(historialDatos[historialDatos.length - 1]);
    }
};

function pasoSim(iteracion) {
    if (!corriendo) return;
    fetch('/step?iter=' + iteracion)
    .then(response => response.json())
    .then(data => {
        if (data.finished) {
            statusDiv.textContent = "Simulación finalizada en iteración " + iteracion + ". Población extinguida.";
            historialDatos = data.historial || historialDatos;
            actualizarGraficasDesdeHistorial(historialDatos);
            mostrarResultadosFinales(historialDatos[historialDatos.length - 1]);
            corriendo = false;
            iniciarBtn.disabled = false;
            detenerBtn.disabled = true;
            descargarJsonBtn.disabled = false;
            descargarCsvBtn.disabled = false;
            return;
        }
        actualizarGraficas(data, iteracion);
        historialDatos.push(data);
        intervaloId = setTimeout(() => pasoSim(iteracion + 1), 200);
    });
}

function actualizarGraficas(data, iteracion){
    Plotly.extendTraces(graphDiv, {
        x: [[iteracion], [iteracion], [iteracion]],
        y: [[data.pasto], [data.presas], [data.depredadores]]
    }, [0, 1, 2]);

    for(let i=0; i<3; i++){
        if(graphDiv.data[i].x.length > maxPuntos){
            graphDiv.data[i].x.shift();
            graphDiv.data[i].y.shift();
        }
    }

    Plotly.extendTraces(statsGraphDiv, {
        x: [[iteracion],[iteracion],[iteracion],[iteracion]],
        y: [[data.nacimientos_presas],[data.muertes_presas],[data.nacimientos_depredadores],[data.muertes_depredadores]]
    }, [0,1,2,3]);

    for(let i=0; i<4; i++){
        if(statsGraphDiv.data[i].x.length > maxPuntos){
            statsGraphDiv.data[i].x.shift();
            statsGraphDiv.data[i].y.shift();
        }
    }
}

function reiniciarGraficas(){
    Plotly.react(graphDiv, [trazaPasto, trazaPresas, trazaDepredadores], diseño);
    Plotly.react(statsGraphDiv, [trazaNacimientosPresas, trazaMuertesPresas, trazaNacimientosDepredadores, trazaMuertesDepredadores], diseñoStats);
}

function actualizarGraficasDesdeHistorial(historial){
    trazaPasto.x = []; trazaPasto.y = [];
    trazaPresas.x = []; trazaPresas.y = [];
    trazaDepredadores.x = []; trazaDepredadores.y = [];

    trazaNacimientosPresas.x = []; trazaNacimientosPresas.y = [];
    trazaMuertesPresas.x = []; trazaMuertesPresas.y = [];
    trazaNacimientosDepredadores.x = []; trazaNacimientosDepredadores.y = [];
    trazaMuertesDepredadores.x = []; trazaMuertesDepredadores.y = [];

    historial.forEach(d => {
        trazaPasto.x.push(d.iteracion);
        trazaPasto.y.push(d.pasto);
        trazaPresas.x.push(d.iteracion);
        trazaPresas.y.push(d.presas);
        trazaDepredadores.x.push(d.iteracion);
        trazaDepredadores.y.push(d.depredadores);

        trazaNacimientosPresas.x.push(d.iteracion);
        trazaNacimientosPresas.y.push(d.nacimientos_presas);
        trazaMuertesPresas.x.push(d.iteracion);
        trazaMuertesPresas.y.push(d.muertes_presas);
        trazaNacimientosDepredadores.x.push(d.iteracion);
        trazaNacimientosDepredadores.y.push(d.nacimientos_depredadores);
        trazaMuertesDepredadores.x.push(d.iteracion);
        trazaMuertesDepredadores.y.push(d.muertes_depredadores);
    });

    Plotly.react(graphDiv, [trazaPasto, trazaPresas, trazaDepredadores], diseño);
    Plotly.react(statsGraphDiv, [trazaNacimientosPresas, trazaMuertesPresas, trazaNacimientosDepredadores, trazaMuertesDepredadores], diseñoStats);
}

function mostrarResultadosFinales(resultados){
    resultadosFinalesDiv.innerHTML = `
        <h2>Resultados Finales</h2>
        <p><strong>Total de Pasto:</strong> ${resultados.pasto}</p>
        <p><strong>Total de Presas:</strong> ${resultados.presas}</p>
        <p><strong>Total de Depredadores:</strong> ${resultados.depredadores}</p>
        <p><strong>Nacimientos Presas:</strong> ${resultados.nacimientos_presas}</p>
        <p><strong>Muertes Presas:</strong> ${resultados.muertes_presas}</p>
        <p><strong>Nacimientos Depredadores:</strong> ${resultados.nacimientos_depredadores}</p>
        <p><strong>Muertes Depredadores:</strong> ${resultados.muertes_depredadores}</p>
    `;
}

descargarJsonBtn.onclick = () => {
    if(historialDatos.length === 0) return;
    const dataStr = "data:text/json;charset=utf-8," + encodeURIComponent(JSON.stringify(historialDatos));
    const dlAnchorElem = document.createElement('a');
    dlAnchorElem.setAttribute("href", dataStr);
    dlAnchorElem.setAttribute("download", "simulacion_datos.json");
    dlAnchorElem.click();
}

descargarCsvBtn.onclick = () => {
    if(historialDatos.length === 0) return;
    const header = ["iteracion","pasto","presas","depredadores","nacimientos_presas","muertes_presas","nacimientos_depredadores","muertes_depredadores"];
    const rows = historialDatos.map(d => [
        d.iteracion,
        d.pasto,
        d.presas,
        d.depredadores,
        d.nacimientos_presas,
        d.muertes_presas,
        d.nacimientos_depredadores,
        d.muertes_depredadores
    ]);
    let csvContent = "data:text/csv;charset=utf-8," + header.join(",") + "\\n" + rows.map(e => e.join(",")).join("\\n");
    const encodedUri = encodeURI(csvContent);
    const dlAnchorElem = document.createElement('a');
    dlAnchorElem.setAttribute("href", encodedUri);
    dlAnchorElem.setAttribute("download", "simulacion_datos.csv");
    dlAnchorElem.click();
}

function actualizarGraficas(data, iteracion) {
    const x = [];
    const y = [];
    const colores = [];



    for (let i = 0; i < TAMANIO_MUNDO; i++) {
        for (let j = 0; j < TAMANIO_MUNDO; j++) {
            x.push(i);
            y.push(j);
            if (data.grilla[i][j].pasto) {
                colores.push('green'); // Pasto
            } else if (data.grilla[i][j].animal && data.grilla[i][j].animal.tipo === PRESA) {
                colores.push('blue'); // Presa
            } else if (data.grilla[i][j].animal && data.grilla[i][j].animal.tipo === DEPREDADOR) {
                colores.push('red'); // Depredador
            } else {
                colores.push('white'); // Vacío
            }
        }
    }



    const trace = {
        x: x,
        y: y,
        mode: 'markers',
        marker: {
            size: 10,
            color: colores,
            line: { width: 1 }
        },
        type: 'scatter'
    };



    const layout = {
        title: 'Estado del Mundo',
        xaxis: { title: 'X' },
        yaxis: { title: 'Y' },
        showlegend: false,
        height: 600,
        width: 600
    };



    Plotly.newPlot('graph', [trace], layout);
}

</script>
</body>
</html>
"""

@app.route('/')
def index():
    return render_template_string(HTML_PAGE)

@app.route('/reset')
def reset():
    global mundo, simulacion_corriendo
    prop_pasto = float(request.args.get('pasto', 0.6))
    prop_presas = float(request.args.get('presas', 0.15))
    prop_depredadores = float(request.args.get('depredadores', 0.05))
    mundo = Mundo(TAMANIO_MUNDO, prop_pasto, prop_presas, prop_depredadores)
    simulacion_corriendo = True
    return ('', 204)

@app.route('/step')
def step():
    global mundo, simulacion_corriendo
    if not simulacion_corriendo:
        return jsonify({"finished": True})
    mundo.paso()
    pasto_cuenta, presa_cuenta, depredador_cuenta = mundo.contar()

    ultimo_registro = mundo.historial[-1] if mundo.historial else {}

    if presa_cuenta == 0 or depredador_cuenta == 0:
        simulacion_corriendo = False
        return jsonify({
            "finished": True,
            "pasto": pasto_cuenta,
            "presas": presa_cuenta,
            "depredadores": depredador_cuenta,
            "nacimientos_presas": ultimo_registro.get('nacimientos_presas', 0),
            "muertes_presas": ultimo_registro.get('muertes_presas', 0),
            "nacimientos_depredadores": ultimo_registro.get('nacimientos_depredadores', 0),
            "muertes_depredadores": ultimo_registro.get('muertes_depredadores', 0),
            "historial": mundo.historial
        })

    return jsonify({
        "finished": False,
        "pasto": pasto_cuenta,
        "presas": presa_cuenta,
        "depredadores": depredador_cuenta,
        "nacimientos_presas": ultimo_registro.get('nacimientos_presas', 0),
        "muertes_presas": ultimo_registro.get('muertes_presas', 0),
        "nacimientos_depredadores": ultimo_registro.get('nacimientos_depredadores', 0),
        "muertes_depredadores": ultimo_registro.get('muertes_depredadores', 0),
    })

if __name__ == '__main__':
    app.run(debug=True)

