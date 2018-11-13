//A javascript copy of Michel van den Bergh's sprta.py script at
//hardy.uhasselt.be/Toga/sprta.py 

var BB = Math.log(10)/400;

function L(x) { 
    return 1 / (1 + Math.exp(-BB * x));
}

function scale(de) {
    return (4 * Math.exp(-BB * de)) / Math.pow(1 + Math.exp(-BB * de), 2);
}

function Sprt(alpha, beta, elo0, elo1, draw_elo) {
    this.draw_elo = draw_elo;
    this.elo0 = elo0;
    this.elo1 = elo1;
    this.alpha = alpha;
    this.beta = beta;
}

Sprt.prototype.wdl = function(elo_diff) {
    var w = L(elo_diff - this.draw_elo)
    var l = L(-elo_diff - this.draw_elo)
    var d = 1 - w - l
    return [w, d, l];
}

Sprt.prototype.llr = function(elo_diff1, elo_diff2) {
    var wdl1 = this.wdl(elo_diff1);
    var wdl2 = this.wdl(elo_diff2);
    return [Math.log(wdl2[0]/wdl1[0]),
            Math.log(wdl2[1]/wdl1[1]),
            Math.log(wdl2[2]/wdl1[2])];
}

Sprt.prototype.mu = function(elo_diff) {
    var wdl = this.wdl(elo_diff);
    var llr = this.llr(this.elo0, this.elo1);
    return wdl[0] * llr[0] + wdl[1] * llr[1] + wdl[2] * llr [2];
}

Sprt.prototype.sigma2 = function(elo_diff) {
    var wdl = this.wdl(elo_diff);
    var llr = this.llr(this.elo0, this.elo1);
    var mu = this.mu(elo_diff);
    var mu2 = wdl[0] * Math.pow(llr[0], 2) + 
              wdl[1] * Math.pow(llr[1], 2) + 
              wdl[2] * Math.pow(llr[2], 2);

    return mu2 - mu * mu;
}

Sprt.prototype.characteristics = function(elo_diff) {
    var mu = this.mu(elo_diff);
    var sigma2 = this.sigma2(elo_diff);
    var coeff = 2 * mu / sigma2;
    var alpha = this.alpha;
    var beta = this.beta;
    var la = Math.log(beta/(1 - alpha));
    var lb = Math.log((1 - beta)/alpha);
    var expa = Math.exp(-coeff * la);
    var expb = Math.exp(-coeff * lb);

    var e;
    var prob_h1;

    if (Math.abs(coeff * (la - lb)) < 1e-6) {
        e = -la * lb / sigma2;
        prob_h1 = (0 - la) / (lb - la);    
    } else {
        e = -(1/mu) * (-la*expb + lb*expa - (lb - la)) / (expb - expa);
        prob_h1 = (1 - expa) / (expb - expa);
    }

    return [prob_h1, e];
}

function main (elo0, elo1, draw_elo, elo) {
    var alpha = 0.05;
    var beta = 0.05;

    var s = new Sprt(alpha, beta, elo0, elo1, draw_elo);
    var delo = elo / scale(draw_elo);

    return s.characteristics(delo);
}
