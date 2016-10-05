from collections import OrderedDict

import common
from common import fresh_name
from target_syntax import *
import library
from syntax_tools import all_types, fresh_var

INDENT = "  "

class CxxPrinter(common.Visitor):

    def __init__(self):
        self.types = OrderedDict()

    def typename(self, t):
        return self.types[t]

    def is_ptr_type(self, t):
        return isinstance(t, THandle)

    def to_ptr(self, x, t):
        return x if self.is_ptr_type(t) else "&({})".format(x)

    def visit_TNative(self, t, name):
        return "{} {}".format(t.name, name)

    def visit_TInt(self, t, name):
        return "int {}".format(name)

    def visit_TLong(self, t, name):
        return "long {}".format(name)

    def visit_TBool(self, t, name):
        return "bool {}".format(name)

    def visit_TRef(self, t, name):
        return self.visit(t.t, "&{}".format(name))

    def visit_TMaybe(self, t, name):
        return self.visit(t.t, name) if self.is_ptr_type(t.t) else self.visit(t.t, "*{}".format(name))

    def visit_THandle(self, t, name):
        return "{} *{}".format(self.typename(t), name)

    def visit_TNativeMap(self, t, name):
        return "std::unordered_map< {}, {} > {}".format(self.visit(t.k, ""), self.visit(t.v, ""), name)

    def visit_TMap(self, t, name):
        return self.visit(t.rep_type(), name)

    def visit_TBag(self, t, name):
        return self.visit(t.rep_type(), name)

    def visit_TRecord(self, t, name):
        return "{} {}".format(self.typename(t), name)

    def visit_TEnum(self, enum, name):
        return "{} {}".format(self.typename(enum), name)

    def visit_TVector(self, t, name):
        return "{}[{}]".format(self.visit(t.t, name), t.n)

    def visit_EVectorGet(self, e, indent=""):
        vsetup, v = self.visit(e.e, indent)
        isetup, i = self.visit(e.i, indent)
        return (vsetup + isetup, "{}[{}]".format(v, i))

    def visit_SForEach(self, for_each, indent):
        id = for_each.id
        iter = for_each.iter
        body = for_each.body
        if isinstance(iter.type, library.TIntrusiveLinkedList):
            return self.visit(iter.type.for_each(id, iter, body), indent)
        raise NotImplementedError()

    def visit_SWhile(self, w, indent):
        cond_setup, cond = self.visit(ENot(w.e), indent+INDENT)
        body = self.visit(w.body, indent=indent+INDENT)
        return "{indent0}for (;;) {{\n{cond_setup}{indent}if ({cond}) break;\n{body}{indent0}}}\n".format(
            indent0=indent,
            indent=indent+INDENT,
            cond_setup=cond_setup,
            cond=cond,
            body=body)

    def visit_SBreak(self, s, indent):
        return "{indent}break;\n".format(indent=indent)

    def visit_str(self, s, indent=""):
        return indent + s + "\n"

    def visit_Query(self, q, indent=""):
        ret_type = q.ret.type
        if isinstance(ret_type, TBag):
            x = EVar(common.fresh_name("x")).with_type(ret_type.t)
            s  = "{indent}template <class F>\n".format(indent=indent)
            s += "{indent}inline void {name} ({args}const F& _callback) const {{\n{body}  }}\n\n".format(
                indent=indent,
                type=self.visit(ret_type, ""),
                name=q.name,
                args="".join("{}, ".format(self.visit(t, name)) for name, t in q.args),
                body=self.visit(SForEach(x, q.ret, "_callback({});".format(x.id)), indent=indent+INDENT))
            return s
        else:
            body, out = self.visit(q.ret, indent+INDENT)
            return "{indent}inline {type} {name} ({args}) const {{\n{body}    return {out};\n  }}\n\n".format(
                indent=indent,
                type=self.visit(ret_type, ""),
                name=q.name,
                args=", ".join(self.visit(t, name) for name, t in q.args),
                out=out,
                body=body)

    def visit_Op(self, q, indent=""):
        s = "{}inline void {} ({}) {{\n{}  }}\n\n".format(
            indent,
            q.name,
            ", ".join(self.visit(t, name) for name, t in q.args),
            self.visit(q.body, indent+INDENT))
        return s

    def visit_ENull(self, e, indent=""):
        return ("", "NULL")

    def visit_EAlterMaybe(self, e, indent=""):
        setup1, ee = self.visit(e.e, indent)
        tmp = fresh_var(e.e.type)
        setup2 = "{indent}{decl} = {val};\n".format(indent=indent, decl=self.visit(tmp.type, tmp.id), val=ee)
        res = fresh_var(e.type)
        setup3 = "{indent}{decl};\n".format(
            indent=indent,
            decl=self.visit(res.type, res.id))
        setup4 = self.visit(SIf(EBinOp(tmp, "==", ENull()), SAssign(res, ENull()), SAssign(res, e.f.apply_to(tmp))), indent=indent)
        return (setup1 + setup2 + setup3 + setup4, res.id)

    def visit_EEmptyList(self, e, indent=""):
        return self.visit(e.type.make_empty(), indent)

    def native_map_get(self, e, default_value, indent=""):
        (smap, emap) = self.visit(e.map, indent)
        (skey, ekey) = self.visit(e.key, indent)
        (sdefault, edefault) = self.visit(default_value, indent)
        iterator = fresh_var(TNative("auto"), "map_iterator")
        res = fresh_var(e.type, "lookup_result")
        s  = "{indent}{declare_res};\n".format(indent=indent, declare_res=self.visit(res.type, res.id))
        s += smap + skey
        s += "{indent}{declare_iterator}({map}.find({key}));\n".format(
            indent=indent,
            declare_iterator=self.visit(iterator.type, iterator.id),
            map=emap,
            key=ekey)
        s += "{indent0}if ({iterator} == {map}.end()) {{\n{sdefault}{indent}{res} = {edefault};\n{indent0}}} else {{\n{indent}{res} = {iterator}->second;\n{indent0}}}\n".format(
            indent0=indent,
            indent=indent+INDENT,
            iterator=iterator.id,
            res=res.id,
            map=emap,
            sdefault=sdefault,
            edefault=edefault)
        return (s, res.id)

    def visit_EMapGet(self, e, indent=""):
        assert isinstance(e.map, EVar)
        value_constructor = self.state_exps[e.map.id].value
        if isinstance(e.map.type, library.TNativeMap):
            return self.native_map_get(e, value_constructor.apply_to(EEmptyList().with_type(value_constructor.arg.type)), indent)
        else:
            return self.visit(e.map.type.get_key(e.map, e.key), indent)

    def visit_SMapUpdate(self, update, indent=""):
        if isinstance(update.change, SNoOp):
            return ""
        if isinstance(update.map.type, library.TNativeMap):
            msetup, map = self.visit(update.map, indent)
            ksetup, key = self.visit(update.key, indent)
            s = "{indent}{decl} = {map}[{key}];\n".format(
                indent=indent,
                decl=self.visit(TRef(update.val_var.type), update.val_var.id),
                map=map,
                key=key)
            return msetup + ksetup + s + self.visit(update.change, indent)
        else:
            return self.visit(update.map.type.update_key(update.map, update.key, update.val_var, update.change), indent)

    def visit_EVar(self, e, indent=""):
        return ("", e.id)

    def visit_EEnumEntry(self, e, indent=""):
        return ("", e.name)

    def visit_EEnumToInt(self, e, indent=""):
        setup, e = self.visit(e.e, indent)
        return (setup, "static_cast<int>(" + e + ")")

    def visit_EBoolToInt(self, e, indent=""):
        setup, e = self.visit(e.e, indent)
        return (setup, "static_cast<int>(" + e + ")")

    def visit_EBinOp(self, e, indent=""):
        op = e.op
        ce1, e1 = self.visit(e.e1, indent)
        ce2, e2 = self.visit(e.e2, indent)
        return (ce1 + ce2, "({e1} {op} {e2})".format(e1=e1, op=op, e2=e2))

    def for_each(self, x, iterable, body, indent=""):
        if isinstance(iterable, EEmptyList):
            return ""
        elif isinstance(iterable, EMap):
            v = EVar(fresh_name()).with_type(iterable.e.type.t)
            return self.for_each(v, iterable.e, seq([
                    SDecl(x.id, iterable.f.apply_to(v)),
                    body]),
                indent=indent)
        elif isinstance(iterable, EFilter):
            return self.for_each(x, iterable.e, SIf(iterable.p.apply_to(x), body, SNoOp()), indent=indent)
        else:
            if type(iterable.type) is TBag:
                return self.for_each_native(x, iterable, body, indent)
            return self.visit(iterable.type.for_each(x, iterable, body), indent=indent)

    def find_one(self, iterable, indent=""):
        if isinstance(iterable, EEmptyList):
            return self.visit(ENull().with_type(TMaybe(iterable.type.t)))
        elif isinstance(iterable, EMap):
            setup, elem = self.find_one(iterable.e, indent)
            res = EVar(fresh_name()).with_type(TMaybe(iterable.e.type.t))
            setup += "{indent}{decl} = {v};\n".format(indent=indent, decl=self.visit(res.type, res.id), v=elem)
            setup2, elem2 = self.visit(iterable.f.apply_to(res), indent)
            return (setup + setup2, elem2)
        elif isinstance(iterable, EFilter):
            # TODO: break OUTERMOST loop
            v = EVar(fresh_name()).with_type(TMaybe(iterable.type.t))
            x = EVar(fresh_name()).with_type(iterable.type.t)
            find = self.for_each(x, iterable, seq([SAssign(v, EJust(x)), SBreak()]), indent=indent)
            s = "{indent}{decl} = {null};\n{find}".format(
                indent=indent,
                decl=self.visit(v.type, v.id),
                null=self.visit(ENull())[1],
                find=find)
            return (s, v.id)
        else:
            if type(iterable.type) is TBag:
                return self.find_one_native(iterable, indent)
            setup, elem = self.visit(iterable.e.type.find_one(e.e), indent)
            return (setup, elem)

    def visit_EUnaryOp(self, e, indent):
        op = e.op
        if op == "the":
            return self.find_one(e.e, indent=indent)
        ce, ee = self.visit(e.e, indent)
        return (ce, "({op} {ee})".format(op=op, ee=ee))

    def visit_EGetField(self, e, indent=""):
        ce, ee = self.visit(e.e, indent)
        op = "."
        if isinstance(e.e.type, THandle) or isinstance(e.e.type, library.TIntrusiveLinkedList):
            op = "->"
        return (ce, "({ee}{op}{f})".format(ee=ee, op=op, f=e.f))

    def visit_ECall(self, e, indent=""):
        setups, args = zip(*[self.visit(arg) for arg in e.args])
        return ("".join(setups), "{func}({args})".format(func=e.func, args=", ".join(args)))

    def visit_Exp(self, e, indent=""):
        raise NotImplementedError(e)

    def visit_SNoOp(self, s, indent=""):
        return ""

    def visit_SAssign(self, s, indent=""):
        cl, el = self.visit(s.lhs, indent)
        cr, er = self.visit(s.rhs, indent)
        return cl + cr + indent + "{} = {};\n".format(el, er)

    def visit_SDecl(self, s, indent=""):
        cv, ev = self.visit(s.val, indent)
        return cv + indent + "{decl} = {ev};\n".format(decl=self.visit(s.val.type, s.id), ev=ev)

    def visit_SSeq(self, s, indent=""):
        return self.visit(s.s1, indent) + self.visit(s.s2, indent)

    def visit_SIf(self, s, indent=""):
        compute_cond, cond = self.visit(s.cond, indent)
        res = """{compute_cond}{indent}if ({cond}) {{\n{then_branch}{indent}}}""".format(
            indent=indent,
            cond=cond,
            compute_cond=compute_cond,
            then_branch=self.visit(s.then_branch, indent + INDENT))
        if not isinstance(s.else_branch, SNoOp):
            res += " else {{\n{else_branch}{indent}}}".format(
                indent=indent,
                else_branch=self.visit(s.else_branch, indent + INDENT))
        return res + "\n"

    def visit_SCall(self, call, indent=""):
        f = getattr(call.target.type, "implement_{}".format(call.func))
        stm = f(call.target, call.args)
        return self.visit(stm, indent)

    def define_type(self, toplevel_name, t, name, indent, sharing):
        if isinstance(t, TEnum):
            return "{indent}enum {name} {{\n{cases}{indent}}};\n".format(
                indent=indent,
                name=name,
                cases="".join("{indent}{case},\n".format(indent=indent+INDENT, case=case) for case in t.cases))
        elif isinstance(t, THandle):
            fields = [("val", t.value_type)]
            s = "{indent}struct {name} {{\n".format(indent=indent, name=name)
            s += "{indent}public:\n".format(indent=indent)
            for (f, ft) in fields:
                s += "{indent}{field_decl};\n".format(indent=indent+INDENT, field_decl=self.visit(ft, f))
            s += "{indent}private:\n".format(indent=indent)
            s += "{indent}friend class {toplevel_name};\n".format(indent=indent+INDENT, toplevel_name=toplevel_name)
            for group in sharing.get(t, []):
                s += "{indent}union {{\n".format(indent=indent+INDENT)
                for gt in group:
                    intrusive_data = gt.intrusive_data(t)
                    s += "{indent}struct {{\n".format(indent=indent+INDENT*2)
                    for (f, ft) in intrusive_data:
                        s += "{indent}{field_decl};\n".format(indent=indent+INDENT*3, field_decl=self.visit(ft, f))
                    s += "{indent}}};\n".format(indent=indent+INDENT*2)
                s += "{indent}}};\n".format(indent=indent+INDENT)
            s += "{indent}}};\n".format(indent=indent)
            return s
        elif isinstance(t, TRecord):
            return "{indent}struct {name} {{\n{fields}{indent}}};\n".format(
                indent=indent,
                name=name,
                fields="".join("{indent}{field_decl};\n".format(indent=indent+INDENT, field_decl=self.visit(t, f)) for (f, t) in t.fields))
        else:
            return ""

    def initial_value(self, t):
        if isinstance(t, TBool):
            return "(false)"
        elif isinstance(t, TInt) or isinstance(t, TLong):
            return "(0)"
        elif isinstance(t, TVector):
            return "{{ {} }}".format(", ".join(self.initial_value(t.t) for i in range(t.n)))
        elif isinstance(t, library.TNativeMap):
            return "()"
        elif self.visit(t, "").endswith("*"): # a little hacky
            return "(NULL)"
        else:
            return self.initial_value(t.rep_type())

    def setup_types(self, spec, state_exps, sharing):
        self.types.clear()
        for name, t in spec.types:
            self.types[t] = name
        handle_types = [t for t in all_types(spec) if isinstance(t, THandle)]
        for t in all_types(spec):
            if t not in self.types and type(t) in [THandle, TRecord, TTuple, TEnum]:
                if isinstance(t, THandle):
                    name = "{}Handle".format(common.capitalize(t.statevar))
                else:
                    name = common.fresh_name("Type")
                self.types[t] = name

    def visit_Spec(self, spec, state_exps, sharing):
        self.state_exps = state_exps

        s = "#pragma once\n"
        s += "#include <unordered_map>\n"
        s += "class {} {{\n".format(spec.name)
        s += "public:\n"

        self.setup_types(spec, state_exps, sharing)
        for t, name in self.types.items():
            s += self.define_type(spec.name, t, name, INDENT, sharing)
        s += "protected:\n"
        for name, t in spec.statevars:
            self.statevar_name = name
            s += "{}{};\n".format(INDENT, self.visit(t, name))
        s += "public:\n"
        s += INDENT + "inline {name}() : {inits} {{ }}\n".format(name=spec.name, inits=", ".join("{} {}".format(name, self.initial_value(t)) for (name, t) in spec.statevars))
        s += INDENT + "{name}(const {name}& other) = delete;\n".format(name=spec.name)
        for op in spec.methods:
            s += self.visit(op, INDENT)
        s += "};"
        return s

class JavaPrinter(CxxPrinter):

    def visit_Spec(self, spec, state_exps, sharing, package=None):
        self.state_exps = state_exps
        self.setup_types(spec, state_exps, sharing)

        s = ""
        if package:
            s += "package {};\n\n".format(package)

        s += "public class {} {{\n".format(spec.name)
        for name, t in spec.types:
            self.types[t] = name
        for name, t in spec.statevars:
            s += "{}{};\n".format(INDENT, self.visit(t, name))
        for e in spec.assumptions:
            s += "public static boolean assumption {{\n {}\n }}\n\n".format(self.visit(e))
        for op in spec.methods:
            s += str(self.visit(op, INDENT))

        # generate auxiliary types
        for t, name in self.types.items():
            s += self.define_type(spec.name, t, name, INDENT, sharing)

        s += "}"
        return s

    def visit_Op(self, q, indent=""):
        s = "{}public void {} ({}) {{\n{}  }}\n\n".format(
            indent,
            q.name,
            ", ".join(self.visit(t, name) for name, t in q.args),
            self.visit(q.body, indent+INDENT))
        return s

    def visit_Query(self, q, indent=""):
        ret_type = q.ret.type
        if isinstance(ret_type, TBag):
            x = EVar(common.fresh_name("x")).with_type(ret_type.t)
            s += "{indent}public void {name} ({args}java.util.function.Consumer<{t}> _callback) {{\n{body}  }}\n\n".format(
                indent=indent,
                type=self.visit(ret_type, ""),
                name=q.name,
                args="".join("{}, ".format(self.visit(t, name)) for name, t in q.args),
                t=self.visit(ret_type.t, ""),
                body=self.visit(SForEach(x, q.ret, "_callback({});".format(x.id)), indent=indent+INDENT))
            return s
        else:
            body, out = self.visit(q.ret, indent+INDENT)
            return "{indent}public {type} {name} ({args}) {{\n{body}    return {out};\n  }}\n\n".format(
                indent=indent,
                type=self.visit(ret_type, ""),
                name=q.name,
                args=", ".join(self.visit(t, name) for name, t in q.args),
                out=out,
                body=body)

    def visit_ENull(self, e, indent=""):
        return ("", "null")

    def visit_EEnumEntry(self, e, indent=""):
        return ("", "{}.{}".format(self.typename(e.type), e.name))

    def visit_EUnaryOp(self, e, indent):
        if e.op == "not":
            setup, ee = self.visit(e.e, indent)
            return (setup, "!({})".format(ee))
        return super().visit_EUnaryOp(e, indent)

    def visit_EBinOp(self, e, indent):
        if e.op == "and":
            setup1, e1 = self.visit(e.e1, indent)
            setup2, e2 = self.visit(e.e2, indent)
            return (setup1 + setup2, "({} && {})".format(e1, e2))
        elif e.op == "or":
            setup1, e1 = self.visit(e.e1, indent)
            setup2, e2 = self.visit(e.e2, indent)
            return (setup1 + setup2, "({} || {})".format(e1, e2))
        elif e.op == "==":
            setup1, e1 = self.visit(e.e1, indent)
            setup2, e2 = self.visit(e.e2, indent)
            return (setup1 + setup2, "java.util.Objects.equals({}, {})".format(e1, e2))
        return super().visit_EBinOp(e, indent)

    def define_type(self, toplevel_name, t, name, indent, sharing):
        if isinstance(t, TEnum):
            return "{indent}enum {name} {{\n{cases}{indent}}};\n".format(
                indent=indent,
                name=name,
                cases="".join("{indent}{case},\n".format(indent=indent+INDENT, case=case) for case in t.cases))
        elif isinstance(t, THandle) or isinstance(t, TRecord):
            public_fields = []
            private_fields = []
            if isinstance(t, THandle):
                public_fields = [("val", t.value_type)]
                for group in sharing.get(t, []):
                    for gt in group:
                        intrusive_data = gt.intrusive_data(t)
                        for (f, ft) in intrusive_data:
                            private_fields.append((f, ft))
            else:
                public_fields = t.fields
            s = "{indent}class {name} {{\n".format(indent=indent, name=name)
            for (f, ft) in public_fields:
                s += "{indent}public {field_decl};\n".format(indent=indent+INDENT, field_decl=self.visit(ft, f))
            for (f, ft) in private_fields:
                s += "{indent}private {field_decl};\n".format(indent=indent+INDENT, field_decl=self.visit(ft, f))
            s += "{indent}}};\n".format(indent=indent)
            return s
        else:
            return ""

    def visit_TBag(self, t, name):
        if hasattr(t, "rep_type"):
            return self.visit(t.rep_type(), name)
        return "java.util.Collection<{}> {}".format(self.visit(t.t, ""), name)

    def visit_THandle(self, t, name):
        return "{} {}".format(self.typename(t), name)

    def visit_TString(self, t, name):
        return "String {}".format(name)

    def visit_EJust(self, e, indent):
        return self.visit(e.e)

    def visit_EGetField(self, e, indent):
        setup, ee = self.visit(e.e, indent)
        return (setup, "({}).{}".format(ee, e.f))

    def for_each_native(self, x, iterable, body, indent):
        setup, iterable = self.visit(iterable, indent)
        return "{setup}{indent}for ({decl} : {iterable}) {{\n{body}}}\n".format(
            indent=indent,
            setup=setup,
            decl=self.visit(x.type, x.id),
            iterable=iterable,
            body=self.visit(body, indent+INDENT))

    def find_one_native(self, iterable, indent):
        it = fresh_name("iterator")
        setup, e = self.visit(iterable, indent)
        return (
            "{setup}{indent}{decl} = {e}.iterator();\n".format(
                setup=setup,
                indent=indent,
                decl=self.visit(TNative("java.util.Iterator<>"), it),
                e=e),
            "({it}.hasNext() ? {it}.next() : null)".format(it=it))

    def visit_TNativeMap(self, t, name):
        return "java.util.HashMap<{}, {}> {}".format(
            self.visit(t.k, ""),
            self.visit(t.v, ""),
            name)

    def visit_TMaybe(self, t, name):
        return self.visit(t.t, name)

    def visit_TRef(self, t, name):
        return self.visit(t.t, name)

    def visit_SMapUpdate(self, update, indent=""):
        if isinstance(update.change, SNoOp):
            return ""
        if isinstance(update.map.type, library.TNativeMap):
            vsetup, val = self.visit(EMapGet(update.map, update.key), indent)
            s = "{indent}{decl} = {val};\n".format(
                indent=indent,
                decl=self.visit(TRef(update.val_var.type), update.val_var.id),
                val=val)
            msetup, map = self.visit(update.map, indent) # TODO: deduplicate
            ksetup, key = self.visit(update.key, indent) # TODO: deduplicate
            return vsetup + s + self.visit(update.change, indent) + msetup + "{indent}{map}.put({key}, {val});\n".format(indent=indent, map=map, key=key, val=update.val_var.id)
        else:
            return super().visit_SMapUpdate(update, indent)

    def native_map_get(self, e, default_value, indent=""):
        (smap, emap) = self.visit(e.map, indent)
        (skey, ekey) = self.visit(e.key, indent)
        (sdefault, edefault) = self.visit(default_value, indent)
        return (smap + skey + sdefault, "{emap}.getOrDefault({ekey}, {edefault})".format(emap=emap, ekey=ekey, edefault=edefault))

    def visit_object(self, o, *args, **kwargs):
        return "/* implement visit_{} */".format(type(o).__name__)
