from .interface import ConcreteImpl, RecordType, NativeTy, FakeObject
from common import fresh_name

class Filtered(ConcreteImpl):
    def __init__(self, ty, fields, qvars, predicate):
        self.ty = ty
        self._fields = fields
        self.qvars = qvars
        self.predicate = predicate
    def __str__(self):
        return "Filtered({})".format(self.ty)
    def __repr__(self):
        return self.__str__()
    def fields(self):
        return self.ty.fields()
    def construct(self, gen, parent_structure):
        return self.ty.construct(gen, parent_structure)
    def needs_var(self, v):
        return self.ty.needs_var(v) or any(vv.name == v for vv in self.predicate.vars() if vv.name in self.qvars)
    def state(self):
        return self.ty.state()
    def private_members(self):
        return self.ty.private_members()
    def matches(self, gen, x):
        return gen.predicate(
            [(f, NativeTy(t)) for (f, t) in self._fields.items()],
            [(v, NativeTy(t)) for (v, t) in self.qvars.items()],
            self.predicate, x)
    def gen_query(self, gen, qvars, parent_structure):
        proc, es = self.ty.gen_query(gen, qvars, parent_structure)
        names = [fresh_name() for s in self.ty.state()]
        mapdict = { }
        for n, (f, t), v in zip(names, self.ty.state(), es):
            proc += gen.decl(n, t, v)
            mapdict[f] = n
        iterator = FakeObject(mapdict)
        proc += gen.while_true(gen.true_value())
        p1, hn = self.ty.gen_has_next(gen, parent_structure, iterator)
        proc += p1
        proc += gen.if_true(gen.not_true(hn))
        proc += gen.break_loop()
        proc += gen.endif()
        p2, cur = self.ty.gen_current(gen, parent_structure, iterator)
        proc += p2
        curN = fresh_name("current")
        proc += gen.decl(curN, RecordType(), cur)
        proc += gen.if_true(self.matches(gen, curN))
        proc += gen.break_loop()
        proc += gen.endif()
        proc += self.ty.gen_advance(gen, parent_structure, iterator)
        proc += gen.endwhile()
        return proc, names
    def gen_empty(self, gen, qvars):
        return self.ty.gen_empty(gen, qvars)
    def gen_find_any(self, gen, parent_structure):
        return self.ty.gen_find_any(gen, parent_structure)
    def gen_current(self, gen, parent_structure, iterator):
        return self.ty.gen_current(gen, parent_structure, iterator)
    def gen_advance(self, gen, parent_structure, iterator):
        # while true:
        #   ty.advance()
        #   break if !ty.hasNext()
        #   break if ty.current() matches filter
        proc  = gen.do_while()
        proc += self.ty.gen_advance(gen, parent_structure, iterator)
        p1, hn = self.ty.gen_has_next(gen, parent_structure, iterator)
        proc += p1
        proc += gen.if_true(gen.not_true(hn))
        proc += gen.break_loop()
        proc += gen.endif()
        p2, n = self.ty.gen_current(gen, parent_structure, iterator)
        proc += p2
        proc += gen.if_true(self.matches(gen, n))
        proc += gen.break_loop()
        proc += gen.endif()
        proc += gen.end_do_while(gen.true_value())
        return proc
    def gen_has_next(self, gen, parent_structure, iterator):
        return self.ty.gen_has_next(gen, parent_structure, iterator)
    def gen_insert(self, gen, x, parent_structure):
        return self.ty.gen_insert(gen, x, parent_structure)
    def gen_remove(self, gen, x, parent_structure):
        return self.ty.gen_remove(gen, x, parent_structure)
    def gen_remove_in_place(self, gen, parent_structure, iterator):
        return self.ty.gen_remove_in_place(gen, parent_structure, iterator)
    def gen_update(self, gen, fields, x, remap, parent_structure):
        return self.ty.gen_update(gen, fields, x, remap, parent_structure)
    def auxtypes(self):
        return self.ty.auxtypes()
