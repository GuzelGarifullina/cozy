import unittest

from cozy.syntax_tools import alpha_equivalent, pprint, mk_lambda
from cozy.target_syntax import *

class TestAlphaEquivalent(unittest.TestCase):

    def test_allow_rename(self):
        x = EHole("x", TInt(), None)
        y = EHole("y", TInt(), None)
        for b in [True, False]:
            aeq = alpha_equivalent(x, y, allow_rename=lambda v1, v2: b)
            assert aeq == b, "{} is {n}alpha-equiv to {}".format(pprint(x), pprint(y), n="" if aeq else "not ")

    def test_binders(self):
        v1 = EVar("foo")
        e1 = EMap(v1, mk_lambda(TInt(), lambda arg: v1))
        e2 = EMap(v1, mk_lambda(TInt(), lambda arg: v1))
        assert e1.f.arg.id != e2.f.arg.id
        assert alpha_equivalent(e1, e2)