import unittest

from cozy.target_syntax import *
from cozy.syntax_tools import *
from cozy.evaluation import eval
from cozy.typecheck import retypecheck

zero = ENum(0).with_type(INT)
one  = ENum(1).with_type(INT)

class TestEvaluation(unittest.TestCase):

    def test_mk_map(self):
        e = EMakeMap(EBinOp(ESingleton(zero), "+", ESingleton(one)),
            mk_lambda(INT, lambda i: zero),
            mk_lambda(TBag(INT), lambda ii: EMakeMap(ii,
                mk_lambda(INT, lambda i: one),
                mk_lambda(TBag(INT), lambda ii: EUnaryOp(UOp.Sum, ii)))))
        assert retypecheck(e)
        m = eval(e, env={})
        assert m[0][1] == 1
        assert 1 not in m.keys()
        assert 0 not in m[0].keys()