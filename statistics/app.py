from flask import Flask, render_template

app = Flask(__name__)

@app.route('/')
def index():
    return render_template('index.html')

@app.route('/t_test')
def t_test():
    return render_template('t_student.html')

@app.route('/chi_square')
def chi_square():
    return render_template('chi_square.html')

@app.route('/anova')
def anova():
    return render_template('anova.html')

@app.route('/jb')
def jb():
    return render_template('jb.html')

@app.route('/shapiro')
def shapiro():
    return render_template('shapiro.html')

@app.route('/kolmo')
def kolmo():
    return render_template('kolmo.html')

@app.route('/mcnemar')
def mcnemar():
    return render_template('mcnemar.html')

@app.route('/cochranq')
def cochranq():
    return render_template('cochranq.html')

@app.route('/wilcoxon')
def wilcoxon():
    return render_template('wilcoxon.html')

if __name__ == '__main__':
    app.run(debug=True)
