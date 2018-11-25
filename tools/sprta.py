from __future__ import division

import math,sys

"""
[W1] A. Wald, Sequential analysis.
"""
bb=math.log(10)/400

def L(x):
    return 1/(1+math.exp(-bb*x))

def scale(de):
    return (4*math.exp(-bb*de))/(1+math.exp(-bb*de))**2

class SPRT:

    def __init__(self,alpha=0.05,beta=0.05,elo0=-1.5,elo1=4.5,draw_elo=240):
        """ 
elos are expressed in BayesElo.
"""
        self.draw_elo=draw_elo
        self.elo0=elo0
        self.elo1=elo1
        self.alpha=alpha
        self.beta=beta
        Ld=L(draw_elo)
        Ldi=L(-draw_elo)


    def wdl(self,elo_diff):
        w=L(elo_diff-self.draw_elo)
        l=L(-elo_diff-self.draw_elo)
        d=1-w-l
        return(w,d,l)

    def _LLR(self,elo_diff1,elo_diff2):
        wdl1=self.wdl(elo_diff1)
        wdl2=self.wdl(elo_diff2)
        return (math.log(wdl2[0]/wdl1[0]),
                math.log(wdl2[1]/wdl1[1]),
                math.log(wdl2[2]/wdl1[2]))

    def _mu(self,elo_diff):
        w,d,l=self.wdl(elo_diff)
        wr,dr,lr=self._LLR(self.elo0,self.elo1)
        return w*wr+d*dr+l*lr

    def _sigma2(self,elo_diff):
        w,d,l=self.wdl(elo_diff)
        wr,dr,lr=self._LLR(self.elo0,self.elo1)
        mu=self._mu(elo_diff)
        mu2=w*wr**2+d*dr**2+l*lr**2
        return mu2-mu**2

    def characteristics(self,elo_diff):
        """ 
Expected running time and power of SPRT test using Brownian approximation.
See e.g. [W1].
"""
        mu=self._mu(elo_diff)
        sigma2=self._sigma2(elo_diff)
        coeff=2*mu/sigma2
        alpha=self.alpha
        beta=self.beta
        LA=math.log(beta/(1-alpha))
        LB=math.log((1-beta)/alpha)
        exp_a=math.exp(-coeff*LA)
        exp_b=math.exp(-coeff*LB)
# avoid division by zero
        if abs(coeff*(LA-LB))<1e-6:
            E=-LA*LB/sigma2
        else:
            E=-(mu**(-1))*(-LA*exp_b+LB*exp_a-(LB-LA))/(exp_b-exp_a)
        
        if abs(coeff*(LA-LB))<1e-6:
            prob_H1=(0-LA)/(LB-LA)
        else:
            prob_H1=(1-exp_a)/(exp_b-exp_a)

        return (prob_H1,E)

if __name__=='__main__':

    if len(sys.argv)!=5:
        print("""Usage: sprta.py elo0 elo1 draw_elo elo
elo0,elo1 are expressed in BayesElo
elo is expressed in LogisticElo
""")
        sys.exit()
    else:
        elo0=float(sys.argv[1])
        elo1=float(sys.argv[2])
        draw_elo=float(sys.argv[3])
        elo=float(sys.argv[4])

    print("elo0=      ",elo0)
    print("elo1=      ",elo1)
    print("draw_elo=  ",draw_elo)
    print("elo=       ",elo)

    alpha=0.05
    beta=0.05

    s=SPRT(alpha,beta,elo0,elo1,draw_elo)
    delo=elo/scale(draw_elo)
    (power,expected)=s.characteristics(delo)

    print("pass probability:      %4.2f%%" % (100*power))
    print("avg running time: %10.0f" % expected)

    
