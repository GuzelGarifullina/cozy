from __future__ import print_function

import re
import tempfile
import os
import subprocess
import sys

import codegen
import predicates
import plans
from structures.interface import TupleTy, This, TupleInstance, IntTy
from common import capitalize, fresh_name, indent, open_maybe_stdout

class CSharpCodeGenerator(codegen.CodeGenerator):
    def __init__(self, identity_equals_types=None):
        super(CSharpCodeGenerator, self).__init__()
        self.identity_equals_types = set(identity_equals_types or ())

    def __str__(self):
        return "CSharpCodeGenerator" 
   
    def _is_primitive(self, ty):
        ty = ty.gen_type(self)
        ty = ty.split('.').pop()
        types = ["Boolean", "Date", "Char", 
                 "String", "Decimal", "Byte", "Short", "Integer", "Long", "Single", "Double"]
        for t in types:
            if t == ty or t.lower == ty:
                return True
        return ty[0] != ty[0].upper()

    def should_use_double_equals_map(self, ty):
        if not isinstance(ty, str):
            ty = ty.gen_type(self)
        if ty in self.identity_equals_types:
            return True
        return False

    def should_use_double_equals(self, ty):
        return self.should_use_double_equals_map(ty) or self._is_primitive(ty)

    def eq(self, ty, e1, e2):
        return ("({}) == ({})" if self.should_use_double_equals(ty) else "({}).equals({})").format(e1, e2)

    def map_type(self, kt, vt):
        return "Dictionary<{}, {}>".format (kt.gen_type(self), vt.gen_type(self))

    def map_handle_type(self, kt, vt):
        return vt.gen_type(self)
    
    def new_map(self, kt, vt):
        kt = kt.gen_type(self)
        if self.should_use_double_equals_map(kt):
           return "new Dictionary<{}, {}>(new IdentityEqualityComparer<{}>())".format(kt, vt.gen_type(self), kt)
        else:
           return "new Dictionary<{}, {}>()".format(kt, vt.gen_type(self))
        
    def map_find_handle(self, m, k, dst):
        return "{}.TryGetValue({}, out {});\n".format(m, k, dst)

    def map_handle_exists(self, m, handle):
        return "{} != null".format(handle)

    def map_read_handle(self, handle):
        return handle

    def map_write_handle(self, m, handle, k, v):
        return self.map_put(m, k, v)

    def map_put(self, m, k, v):
        return "{}[{}] = {};".format(m, k, v)
    
    def for_each_map_entry(self, m, keyType, valType, body):
        entryname = fresh_name("entry")
        kname = fresh_name("key")
        vname = fresh_name("val")
        return """foreach (KeyValuePair<{kt}, {vt}> {e} in {m}) {{
            {kt} {k} = e.Key;
            {vt} {v} = e.Value;
            {body}
        }}\n""".format(
            kt=keyType.gen_type(self), vt=valType.gen_type(self),
            k=kname, v=vname,
            m=m, e=entryname,
            body=body(kname, vname, self.break_loop))

    def stack_type(self, t):
        return "Stack<{}>".format(t.gen_type(self))

    def new_stack(self, t):
        return "new Stack<{}>()".format(t.gen_type(self))
    
    def stack_size_hint(self, stk, n):
        return ""

    def stack_is_empty(self, stk):
        return "({}.Count == 0)".format(stk)

    def stack_push(self, stk, x):
        return "{}.Push({});\n".format(stk, x)

    def stack_pop(self, stk):
        return "{}.Pop();\n".format(stk)

    def stack_peek(self, stk):
        return "{}.Peek()".format(stk)

    def array_type(self, ty):
        return "{}[]".format(ty.gen_type(self))

    def new_array(self, ty, count):
        return "new {}[{}]".format(ty.gen_type(self), count)

    def array_get(self, a, n):
        return "{}[{}]".format(a, n)

    def array_set(self, a, n, v):
        return "{}[{}] = {};\n".format(a, n, v)

    def array_size(self, a):
        return "{}.Length".format(a)

    def array_copy(self, ty, asrc, adst, src_start=0, dst_start=0, amt=None):
        if amt is None:
            amt = self.array_size(asrc)
        return "System.Array.Copy({src}, {src_start}, {dst}, {dst_start}, {amt});\n".format(
            src=asrc, dst=adst, src_start=src_start, dst_start=dst_start, amt=amt)
    
    def vector_type(self, ty, n):
        return self.array_type(ty)
    
    def new_vector(self, ty, n):
        return self.new_array(ty, n)
    
    def vector_init_elem(self, v, ty, i):
        if isinstance(ty, TupleTy) and len(ty.fields) > 1: # a bit of a hack
            return self.vector_set(v, i, self.alloc(ty, []))
        return ""
     
    def vector_get(self, v, i):
        return self.array_get(v, i)
    
    def vector_set(self, v, i, x):
        return self.array_set(v, i, x)

    def alloc(self, ty, args):
        return "new {}({})".format(ty.gen_type(self), ", ".join(args))
    
    def free(self, ty, x):
        return ""
    
    def initialize(self, ty, lval):
        if type(ty) is TupleTy and len(ty.fields) > 1:
            return "{lval} = {new};\n".format(lval=lval, new=self.alloc(ty, []))
        else:
            return ""
    
    def init_new(self, target, ty):
        return self.set(target, "new {}()".format(ty.gen_type(self)))
    
    def node_type(self, node_name):
        return node_name

    def list_type(self, ty):
        return "List<{}>".format(ty)

    def list_get(self, li, index):
        return "({})[{}]".format(li, index)

    def list_add(self, li, item):
        return "({}).Add({});\n".format(li, item)

    def list_add_at_index(self, li, index, item):
        return "({}).Insert({}, {});\n".format(li, index, item)
    
    def list_remove(self, li, item):
        return "({}).Remove({})".format(li, item)

    def list_set(self, li, index, item):
        return "({})[{}] = {};\n".format(li, index, item)

    def list_size(self, li):
        return "({}).Count".format(li)

    def new_linked_list(self, ty):
        return "new LinkedList<{}>()".format(ty)
#------------------------------------------------------------------
    
    def min(self, ty, x, y):
        return "Math.Min({x}, {y})".format(x=x, y=y)
     
    def max(self, ty, x, y):
        return "Math.Max({x}, {y})".format(x=x, y=y)

    def abs(self, e):
        return "Math.Abs({})".format(e)
    
    def left_shift(self, lhs, rhs):
        return "(({}) << ({}))".format(lhs, rhs)

    def right_logic_shift(self, lhs, rhs):
        return "((int)((uint)({}) >> ({})))".format(lhs, rhs)
    def bit_lshr(self, e1, e2):
        return "((int)((uint)({}) >> ({})))".format(e1, e2)

    def bitwise_and(self, lhs, rhs):
        return "(({}) & ({}))".format(lhs, rhs)

    def bitwise_or(self, lhs, rhs):
        return "(({}) | ({}))".format(lhs, rhs)
#-----------------------------------------------------
    
    def plus_one(self, v):
        return "{}++;\n".format(v)

    def end_return(self):
        return "return;\n"

    def record_name(self, r):
        return "({}).Name".format(r)

    def get_node_values(self, node):
        return "{}.Values".format(node)

    def get_node_is_leaf_value(self, node):
        return "{}.IsLeaf()".format(node)

    def get_node_signature(self, node):
        return "{}.Signature".format(node)

    def get_node_next(self, node):
        return "{}.Next".format(node)
#------------------------------------------------------------------ 
    def hash_code(self, Ty, v):
        return _hash_code(Ty.__str__(), "({})".format(v))

    def hash1(self, ty, value):
        return _hash_code(ty.gen_type(self), value)

    def write(self, fields, queries, csharp_namespace=None, csharp_class="DataStructure", csharp="-", **kwargs):
        with open_maybe_stdout(csharp) as f:
            writer = f.write

            writer("using System;\n")
            writer("using System.Collections;\n")
            writer("using System.Collections.Generic;\n")
            writer("using System.Text;\n")
            writer("using System.Runtime.CompilerServices;\n\n")

            if csharp_namespace:
                writer("namespace {} {{\n\n".format(csharp_namespace))

            if self.identity_equals_types is not None:
                _gen_comparator(writer)

            writer("[SerializableAttribute]\n")
            writer("public class {} {{\n".format(csharp_class))

            # record type
            private_members = []
            RECORD_NAME = self.record_type()
            for q in queries:
                private_members += list((f, ty.gen_type(self)) for f, ty in q.impl.private_members())
            _gen_record_type(RECORD_NAME, list(fields.items()), private_members, writer)

            # auxiliary type definitions
            seen = set()
            for q in queries:
                for t in q.impl.auxtypes():
                    _gen_aux_type(t, self, writer, seen)

            # constructor
            writer("  public {}() {{\n".format(csharp_class))
            for q in queries:
                writer(indent("    ", q.impl.construct(self, This())))
            writer("}\n")

            # get current size
            writer("  int my_size = 0;\n")
            writer("  int Size() { return my_size; }\n")

            # add routine
            writer("  public void Add({} x) {{\n".format(RECORD_NAME))
            writer("    ++my_size;\n")
            for q in queries:
                writer(indent("    ", q.impl.gen_insert(self, "x", This())))
            writer("\n  }\n")

            # remove routine
            writer("  public void Remove({} x) {{\n".format(RECORD_NAME))
            writer("    --my_size;\n")
            for q in queries:
                writer(indent("    ", q.impl.gen_remove(self, "x", This())))
            writer("}\n")

            # update routines
            for f, ty in fields.items():
                writer("  public void Update{}({} x, {} val) {{\n".format(capitalize(f), self.record_type(), ty))
                writer("    if ({} != val) {{\n".format(self.get_field("x", f)))
                for q in queries:
                    writer(indent("        ", q.impl.gen_update(self, fields, "x", {f: "val"}, This())))
                writer("      {} = val;\n".format(self.get_field("x", f)))
                writer("    }\n")
                writer("  }\n")
            writer("  public void Update({} x, {}) {{\n".format(self.record_type(), ", ".join("{} {}".format(ty, f) for f, ty in fields.items())))
            for q in queries:
                writer(indent("    ", q.impl.gen_update(self, fields, "x", {f:f for f in fields}, This())))
            for f, ty in fields.items():
                writer("    {} = {};\n".format(self.get_field("x", f), f))
            writer("  }\n")

            # query routines
            for q in queries:
                for f, ty in q.impl.fields():
                    writer("  internal {} {};\n".format(ty.gen_type(self), f))

                it_name = "{}Enumerator".format(q.name)
                writer("  internal sealed class {} : System.Collections.Generic.IEnumerator<{}> {{\n".format(it_name, RECORD_NAME))
                state = q.impl.state()
                writer("    {} parent;\n".format(csharp_class))
                vars_needed = [(v, ty) for v, ty in q.vars if q.impl.needs_var(v)]

                for v, ty in q.impl.fields():
                    writer("    readonly {ty} {v};\n".format(ty=ty.gen_type(self), v=v))
                for v, ty in q.vars:
                    writer("    readonly {} {};\n".format(ty, v))
                for f, ty in state:
                    writer("    {} {};\n".format(ty.gen_type(self), f))

                #constructor
                writer("    internal {}({} parent{}) {{\n".
                       format(it_name, csharp_class, 
                              "".join(", {} {}".format(ty, v) for v,ty in q.vars)))
                writer("      this.parent = parent;\n")
                
                for v, ty in q.impl.fields():
                    writer("      this.{v} = parent.{v};\n".format(ty=ty.gen_type(self), v=v))
                for v, ty in q.vars:
                    writer("      this.{v} = {v};\n".format(v=v))

                writer ("      Reset();\n")
                writer("    }\n")

                writer("    public bool MoveNext() {\n")
                proc, ret = q.impl.gen_has_next(self, parent_structure=TupleInstance("parent"), iterator=This())
                writer(indent("      ", proc))
                writer("return {};\n".format(ret))
                writer("    }\n")

                writer("    public {} Current {{\n".format(RECORD_NAME))
                writer("    get{\n")
                proc, ret = q.impl.gen_next(self, parent_structure=TupleInstance("parent"), iterator=This())
                writer(indent("      ", proc))
                writer("return {};\n".format(ret))
                writer("      }\n")
                writer("    }\n")
                
                writer("    object IEnumerator.Current {\n")
                writer("    get{\n")
                proc, ret = q.impl.gen_next(self, parent_structure=TupleInstance("parent"), iterator=This())
                writer(indent("      ", proc))
                writer("return {};\n".format(ret))
                writer("      }\n")
                writer("    }\n")

                writer("    public void Reset() {\n")
                proc, stateExps = q.impl.gen_query(self, q.vars, This())
                writer(indent("      ", proc))
                writer("\n")
                for (f, ty), f2 in zip(state,stateExps) :
                    writer("      this.{f} = {f2};\n".format(f=f, f2=f2))
                writer("  }\n")

                writer("  public void Dispose() {}\n")
                writer("  }\n")

                writer("  public System.Collections.Generic.IEnumerator <{}> {}({}) {{\n".format(RECORD_NAME, q.name, ", ".
                                                                     join("{} {}".format(ty, v) for v,ty in q.vars)))
                writer("      return new {}(this{});\n".format(it_name, "".join(", {}".format(v) for v, ty in q.vars)))
                writer("  }\n")

                writer("  public {} {}_1({}) {{\n".format(RECORD_NAME, q.name, ", ".join("{} {}".format(ty, v) for v,ty in q.vars)))
                proc, result = q.impl.gen_query_one(self, q.vars, This())
                writer(indent("      ", proc))
                writer("return {};\n".format(result))
                writer("  }\n")
            writer("}\n")

            if csharp_namespace:
                 writer("}\n")

    def supports_cost_model_file(self, f):
        return f.endswith(".cs")

    def dynamic_cost(self, fields, queries, impls, cost_model_file, args):
        for q, i in zip(queries, impls):
            q.impl = i

        tmp = tempfile.mkdtemp()

        self.write(fields, queries, csharp_class="DataStructure", csharp=os.path.join(tmp, "DataStructure.cs"))

        mainFileName = "Program.cs"
        with open(os.path.join(tmp, mainFileName), "w") as f:
            f.write("using System;\n")
            f.write("using System.Collections;\n")
            f.write("using System.Collections.Generic;\n")
            f.write("using  System.Diagnostics;\n");

            f.write("\nclass Program {\n")
            f.write("static void Main(string[] args) { new Program().run(); }\n")
            with open(cost_model_file, "r") as b:
                f.write(b.read())
            f.write("\n}\n")
        orig = os.getcwd()
        os.chdir(tmp)
        cmd = ["csc", "/out:My.exe", "*.cs"]
        with open(os.devnull, "w") as f: 
            ret = subprocess.call(cmd, stdout=f)
            assert ret == 0, "error in {}".format(tmp)
        csharp = subprocess.Popen(["My.exe"], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        stdout, stderr = csharp.communicate()
        if csharp.returncode != 0:
            print("exit status was {} (running in {})".format(csharp.returncode, tmp), file=sys.stderr)
            print(stdout, file=sys.stderr)
            print(stderr, file=sys.stderr)
            raise Exception()
        score = long(stdout.strip()) #was int
        os.chdir(orig)
        return score
    def extensions(self, old):
        return old

def _hash_code(ty, exp):
        return "{}.GetHashCode()".format(exp)

def _gen_aux_type(ty, gen, writer, seen):
    if ty in seen:
        return
    seen.add(ty)
    if type(ty) is TupleTy:
        for _, t in ty.fields.items():
            _gen_aux_type(t, gen, writer, seen)
        writer("[SerializableAttribute]\n")
        writer("    internal class {} {{\n".format(ty.name))
        for f, t in ty.fields.items():
            writer("        {} {};\n".format(t.gen_type(gen), f))
        writer("        public override int GetHashCode() {\n")
        writer("            int hc = 0;\n")
        for f, t in ty.fields.items():
            writer("            hc = 31 * hc + {};\n".format(_hash_code(t.gen_type(gen), f)))
        writer("            return hc;\n")
        writer("        }\n")
        writer("        public override bool Equals(Object other) {\n")
        writer("            if (other == null || GetType() != other.GetType()) return false;\n")
        writer("            {t} x = ({t})other;\n".format(t=ty.name))
        for f in ty.fields:
            writer("            if ({}) return false;\n".format(gen.not_same("x.{}".format(f), f)))
        writer("            return true;\n")
        writer("        }\n")
        writer("    }\n")

def _gen_record_type(name, fields, private_fields, writer):
    writer("[SerializableAttribute]\n")
    writer("    public class {} {{\n".format(name))
    for f,ty in fields:
        writer("        public {} {} {{ get; set; }}\n".format(ty, f))
    for f,ty in private_fields:
        writer("        internal {} {};\n".format(ty, f))
    writer("        public {}({}) {{\n".format(name, ", ".join("{} {}".format(ty, f) for f,ty in fields)))
    for f,ty in fields:
        writer("            this.{f} = {f};\n".format(f=f))
    writer("        }\n")
    writer("        public override string ToString() {\n")
    writer('            return new StringBuilder().Append("{}(")'.format(name))
    first = True
    for f,ty in fields:
        if not first:
            writer(".Append(',')")
        writer('.Append("{}=")'.format(f))
        writer(".Append({})".format(f))
        first = False
    writer(".Append(')').ToString();\n")
    writer("        }\n")
    writer("    }\n")

def _gen_comparator(writer):
    writer("""public class IdentityEqualityComparer<T> : IEqualityComparer<T>
    where T : class
{
    public int GetHashCode(T value)
    {
        return RuntimeHelpers.GetHashCode(value);
    }

    public bool Equals(T left, T right)
    {
        return left == right; // Reference identity comparison
    }
}\n""")