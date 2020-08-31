# pylint:disable=multiple-statements,line-too-long,bad-whitespace,using-constant-test,superfluous-parens,chained-comparison,too-many-branches,bad-continuation,too-many-arguments,too-many-locals,consider-using-in
# Term
# ====
import json as JSON
JSON.stringify = JSON.dumps

def Var(indx): return {'ctor': 'Var', 'indx': indx}
def Ref(name): return {'ctor': 'Ref', 'name': name}
def Typ(): return {'ctor': 'Typ'}
def All(eras,self,name,bind,body): return {'ctor': 'All', 'eras': eras, 'self': self, 'name': name, 'bind': bind, 'body': body}
def Lam(eras,name,body): return {'ctor': 'Lam', 'eras': eras, 'name': name, 'body': body}
def App(eras,func,argm): return {'ctor': 'App', 'eras': eras, 'func': func, 'argm': argm}
def Let(name,expr,body): return {'ctor': 'Let', 'name': name, 'expr': expr, 'body': body}
def Ann(done,expr,type): return {'ctor': 'Ann', 'done': done, 'expr': expr, 'type': type}
def Loc(fr0m,upto,expr): return {'ctor': 'Loc', 'fr0m': fr0m, 'upto': upto, 'expr': expr}

# List
# ====

def Nil(): return {'ctor': 'Nil', 'size': 0}
def Ext(head,tail): return {'ctor': 'Ext', 'head': head, 'tail': tail, 'size': tail.size+1}

# Finds first value satisfying `cond` in a list
def find(list, cond, indx = 0) :
    case = ['(list.ctor)']
    if True:
        if False:
            raise AssertionError
        elif case == 'Nil':
            return None
        elif case == 'Ext':
            if (cond(list.head, indx)) :
                return {"value":list.head, "index":indx}
            else:
                return find(list.tail, cond, indx + 1)
        else:
            raise ValueError(case)
    else:
        raise AssertionError

# Syntax
# ======

def stringify(term, depth = 0) :
    case = ['(term.ctor)']
    if True:
        if False:
            raise AssertionError
        elif case == 'Var':
            return term.indx.split('#')[0]
        elif case == 'Ref':
            return term.name
        elif case == 'Typ':
            return '*'
        elif case == 'All':
            bind = 'Ɐ' if term.eras else 'Π'
            self = term.self or ('x'+(depth+0))
            name = term.name or ('x'+(depth+1))
            type = stringify(term.bind, depth)
            body = stringify(term.body(Var(self+'#'), Var(name+'#')), depth+2)
            return bind + self + '(' + name + ':' + type + ') ' + body
        elif case == 'Lam':
            bind = 'Λ' if term.eras else 'λ'
            name = term.name or ('x'+(depth+0))
            body = stringify(term.body(Var(name+'#')), depth)
            return bind + name + ' ' + body
        elif case == 'App':
            open = '<' if term.eras else '('
            func = stringify(term.func, depth)
            argm = stringify(term.argm, depth)
            clos = '>' if term.eras else ')'
            return open + func + ' ' + argm + clos
        elif case == 'Let':
            name = term.name or ('x'+(depth+0))
            expr = stringify(term.expr, depth)
            body = stringify(term.body(Var(name+'#')), depth+1)
            return '$' + name + '=' + expr + ';' + body
        elif case == 'Ann':
            type = stringify(term.type, depth)
            expr = stringify(term.expr, depth)
            return ':' + type + ' ' + expr
        elif case == 'Loc':
            return stringify(term.expr, depth)
        else:
            raise ValueError(case)
    else:
        raise AssertionError

def parse(code, indx, mode = 'defs') :  # pylint:BUG:disable=unused-argument
    def is_name(chr) :
        val = chr.charCodeAt(0)
        return ((val >= 46 and val < 47) # .
                or (val >= 48 and val < 58) # 0-9
                or (val >= 65 and val < 91) # A-Z
                or (val >= 95 and val < 96) # _
                or (val >= 97 and val < 123)) # a-z
    def parse_name() :
        if (indx < code.length and is_name(code[indx])) :
            result = code[indx] + parse_name()
            indx += 1
            return result
        else :
            return ''
    def parse_nuls() :
        while (code[indx] == ' ' or code[indx] == '\n') :
            indx += 1
    def parse_char(chr) :
        if (indx >= code.length) :
            raise Exception( 'Unexpected eof.' )
        elif (code[indx] != chr) :
            raise Exception( 'Expected ''+chr+'', found '+JSON.stringify(code[indx])+' at '+indx+'.' )
        indx += 1
    def parse_term() :
        parse_nuls()
        chr = code[indx]
        indx += 1
        case = ['(chr)']
        if True:
            if False:
                raise AssertionError
            elif case == '*':
                return lambda ctx: ( Typ() )
            elif (case == 'Ɐ' or
                  case == 'Π'
            ):
                eras = chr == 'Ɐ'
                self = parse_name()
                parse_char('(')
                name = parse_name()
                parse_char(':')
                bind = parse_term()
                parse_char(')')
                body = parse_term()
                return lambda ctx: ( All(eras, self, name, bind(ctx), lambda s,x: ( body(Ext([name,x],Ext([self,s],ctx)))) ) )
            elif (
                    case == 'λ' or
                    case == 'Λ'
            ):
                eras = chr == 'Λ'
                name = parse_name()
                body = parse_term()
                return lambda ctx: ( Lam(eras, name, lambda x: ( body(Ext([name,x],ctx))) ) )
            elif (
                    case == '(' or
                    case == '<'
            ):
                eras = chr == '<'
                func = parse_term()
                argm = parse_term()
                parse_char('>' if eras else ')')
                return lambda ctx: ( App(eras, func(ctx), argm(ctx)) )
            elif case == '$':
                name = parse_name()
                parse_char('=')
                expr = parse_term()
                parse_char(';')
                body = parse_term()
                return lambda ctx: ( Let(name, expr(ctx), lambda x: ( body(Ext([name,x],ctx))) ) )
            elif case == ':':
                type = parse_term()
                expr = parse_term()
                return lambda ctx: ( Ann(False, expr(ctx), type(ctx)) )
            else:
                if (is_name(chr)) :
                    name = chr + parse_name()
                    def _(ctx):
                        got = find(ctx, lambda x: (x[0] == name) )
                        return got.value[1] if got else Ref(name)
                    return _
                else :
                    raise Exception( 'Unexpected symbol: '' + chr + ''.' )
        else:
            raise AssertionError
    def parse_defs() :
        parse_nuls()
        name = parse_name()
        if (name.length > 0) :
            parse_char(':')
            type = parse_term()(Nil())
            term = parse_term()(Nil())
            defs[name] = {type, term}
            parse_defs()
    indx = 0
    if (mode == 'defs') :
        defs = {}
        parse_defs()
        return {defs}
    else :
        return parse_term()(Nil())

# Evaluation
# ==========

def reduce(term, defs, erased = False) :
    case = ['(term.ctor)']
    if True:
        if False:
            raise AssertionError
        elif case == 'Var':
            return Var(term.indx)
        elif case == 'Ref':
            if (defs[term.name]) :
                got = defs[term.name].term
                if (got.ctor == 'Loc' and got.expr.ctor == 'Ref' and got.expr.name == term.name) :
                    return got
                else :
                    return reduce(got, defs, erased)
            else :
                return Ref(term.name)
        elif case == 'Typ':
            return Typ()
        elif case == 'All':
            eras = term.eras
            self = term.self
            name = term.name
            bind = term.bind
            body = term.body
            return All(eras, self, name, bind, body)
        elif case == 'Lam':
            if (erased and term.eras) :
                return reduce(term.body(Lam(False, '', lambda x: (x))), defs, erased)
            else :
                eras = term.eras
                name = term.name
                body = term.body
                return Lam(eras, name, body)
        elif case == 'App':
            if (erased and term.eras) :
                return reduce(term.func, defs, erased)
            else :
                eras = term.eras
                func = reduce(term.func, defs, erased)
                case = ['(func.ctor)']
                if True:
                    if False:
                        raise AssertionError
                    elif case == 'Lam':
                        return reduce(func.body(term.argm), defs, erased)
                    else:
                        return App(eras, func, term.argm)
                else:
                    raise AssertionError
        elif case == 'Let':
            name = term.name
            expr = term.expr
            body = term.body
            return reduce(body(expr), defs, erased)
        elif case == 'Ann':
            return reduce(term.expr, defs, erased)
        elif case == 'Loc':
            return reduce(term.expr, defs, erased)
        else:
            raise ValueError(case)
    else:
        raise AssertionError


def normalize(term, defs, erased = False, seen = None) :
    if seen is None:
        seen = {}
    norm = reduce(term, defs, erased)
    term_hash = hash(term)
    norm_hash = hash(norm)
    if (seen[term_hash] or seen[norm_hash]) :
        return term
    else :
        seen = {term_hash: True, norm_hash: True, **seen}
        norm = reduce(term, defs, erased)
        case = ['(norm.ctor)']
        if True:
            if False:
                raise AssertionError
            elif case == 'Var':
                return Var(norm.indx)
            elif case == 'Ref':
                return Ref(norm.name)
            elif case == 'Typ':
                return Typ()
            elif case == 'All':
                eras = norm.eras
                self = norm.self
                name = norm.name
                bind = normalize(norm.bind, defs, erased, seen)
                body = lambda s,x: ( normalize(norm.body(s,x), defs, erased, seen) )
                return All(eras, self, name, bind, body)
            elif case == 'Lam':
                eras = norm.eras
                name = norm.name
                body = lambda x: ( normalize(norm.body(x), defs, erased, seen) )
                return Lam(eras, name, body)
            elif case == 'App':
                eras = norm.eras
                func = normalize(norm.func, defs, erased, seen)
                argm = normalize(norm.argm, defs, erased, seen)
                return App(eras, func, argm)
            elif case == 'Let':
                return normalize(norm.body(norm.expr), defs, erased, seen)
            elif case == 'Ann':
                return normalize(norm.expr, defs, erased, seen)
            elif case == 'Loc':
                return normalize(norm.expr, defs, erased, seen)
            else:
                raise ValueError(case)
        else:
            raise AssertionError

# Equality
# ========

# Computes the hash of a term. JS strings are hashed, so we just return one.
def hash(term, dep = 0) :
    case = ['(term.ctor)']
    if True:
        if False:
            raise AssertionError
        elif case == 'Var':
            indx = int(term.indx.split('#')[1])
            if (indx < 0) :
                return '^'+(dep+indx)
            else :
                return '#'+indx
        elif case == 'Ref':
            return '$' + term.name
        elif case == 'Typ':
            return 'Type'
        elif case == 'All':
            bind = hash(term.bind, dep)
            body = hash(term.body(Var('#'+(-dep-1)), Var('#'+(-dep-2))), dep+2)
            return 'Π' + term.self + bind + body
        elif case == 'Lam':
            body = hash(term.body(Var('#'+(-dep-1))), dep+1)
            return 'λ' + body
        elif case == 'App':
            func = hash(term.func, dep)
            argm = hash(term.argm, dep)
            return '@' + func + argm
        elif case == 'Let':
            expr = hash(term.expr, dep)
            body = hash(term.body(Var('#'+(-dep-1))), dep+1)
            return '$' + expr + body
        elif case == 'Ann':
            expr = hash(term.expr, dep)
            return expr
        elif case == 'Loc':
            expr = hash(term.expr, dep)
            return expr
        else:
            raise ValueError(case)
    else:
        raise AssertionError

# Are two terms equal?
def equal(a, b, defs, dep = 0, seen = None) :
    if seen is None:
        seen = {}
    a1 = reduce(a, defs, True)
    b1 = reduce(b, defs, True)
    ah = hash(a1)
    bh = hash(b1)
    id = ah + '==' + bh
    if (ah == bh or seen[id]) :
        return True
    else :
        seen[id] = True
        case = ['(a1.ctor', '+', 'b1.ctor)']
        if True:
            if False:
                raise AssertionError
            elif case == 'AllAll':
                a1_body = a1.body(Var('#'+(dep)), Var('#'+(dep+1)))
                b1_body = b1.body(Var('#'+(dep)), Var('#'+(dep+1)))
                return (
                        a1.eras == b1.eras
                        and a1.self == b1.self
                        and equal(a1.bind, b1.bind, defs, dep+0, seen)
                        and equal(a1_body, b1_body, defs, dep+2, seen)
                )
            elif case == 'LamLam':
                a1_body = a1.body(Var('#'+(dep)))
                b1_body = b1.body(Var('#'+(dep)))
                return (
                        a1.eras == b1.eras
                        and equal(a1_body, b1_body, defs, dep+1, seen)
                )
            elif case == 'AppApp':
                return (
                        a1.eras == b1.eras
                        and equal(a1.func, b1.func, defs, dep, seen)
                        and equal(a1.argm, b1.argm, defs, dep, seen)
                )
            elif case == 'LetLet':
                a1_body = a1.body(Var('#'+(dep)))
                b1_body = b1.body(Var('#'+(dep)))
                return (
                        equal(a1.expr, b1.expr, defs, dep+0, seen)
                        and equal(a1_body, b1_body, defs, dep+1, seen)
                )
            elif case == 'AnnAnn':
                return equal(a1.expr, b1.expr, defs, dep, seen)
            elif case == 'LocLoc':
                return equal(a1.expr, b1.expr, defs, dep, seen)
            else:
                return False
        else:
            raise AssertionError


# Type-Checking
# =============

def Err(loc, ctx, msg):
    return {
        'loc': loc,
        'ctx': ctx,
        'msg': msg,
    }

def typeinfer(term, defs, show = stringify, ctx = None, locs = None) :
    if ctx is None:
        ctx = Nil()
    case = ['(term.ctor)']
    if True:
        if False:
            pass
        elif case == 'Var':
            return Var(term.indx)
        elif case == 'Ref':
            got = defs[term.name]
            if (got) :
                return got.type
            else :
                raise Exception( lambda : ( Err(locs, ctx, 'Undefined reference '' + term.name + ''.') ) )
        elif case == 'Typ':
            return Typ()
        elif case == 'App':
            func_typ = reduce(typeinfer(term.func, defs, show, ctx), defs)
            case = ['(func_typ.ctor)']
            if True:
                if False:
                    pass
                elif case == 'All':
                    self_var = Ann(True, term.func, func_typ)
                    name_var = Ann(True, term.argm, func_typ.bind)
                    typecheck(term.argm, func_typ.bind, defs, show, ctx)
                    term_typ = func_typ.body(self_var, name_var)
                    if (func_typ.ctor == 'All' and term.eras != func_typ.eras) :
                        raise Exception( lambda : ( Err(locs, ctx, 'Mismatched erasure.') ) )
                    return term_typ
                else:
                    raise Exception( lambda : ( Err(locs, ctx, 'Non-function application.') ) )
        elif case == 'Let':
            expr_typ = typeinfer(term.expr, defs, show, ctx)
            expr_var = Ann(True, Var(term.name+'#'+(ctx.size+1)), expr_typ)
            body_ctx = Ext({'name':term.name,'type':expr_var['type']}, ctx)
            body_typ = typeinfer(term.body(expr_var), defs, show, body_ctx)
            return body_typ
        elif case == 'All':
            self_var = Ann(True, Var(term.self+'#'+ctx.size), term)
            name_var = Ann(True, Var(term.name+'#'+(ctx.size+1)), term.bind)
            body_ctx = Ext({'name':term.self,'type':self_var['type']}, ctx)
            body_ctx = Ext({'name':term.name,'type':name_var['type']}, body_ctx)
            typecheck(term.bind, Typ(), defs, show, ctx)
            typecheck(term.body(self_var,name_var), Typ(), defs, show, body_ctx)
            return Typ()
        elif case == 'Ann':
            if (not term.done) :
                typecheck(term.expr, term.type, defs, show, ctx)
            return term.type
        elif case == 'Loc':
            locs = {'fr0m': term.fr0m, 'upto': term.upto}
            return typeinfer(term.expr, defs, show, ctx, locs)
    raise Exception( lambda : ( Err(locs, ctx, "Can't infer type.") ) )

def typecheck(term, type, defs, show = stringify, ctx = None, locs = None) :
    if ctx is None:
        ctx = Nil()
    typv = reduce(type, defs)
    case = ['(term.ctor)']
    if True:
        if False:
            pass
        elif case == 'Lam':
            if (typv.ctor == 'All') :
                self_var = Ann(True, term, type)
                name_var = Ann(True, Var(term.name+'#'+(ctx.size+1)), typv.bind)
                body_typ = typv.body(self_var, name_var)
                if (term.eras != typv.eras) :
                    raise Exception( lambda : ( Err(locs, ctx, 'Type mismatch.') ) )
                body_ctx = Ext({'name':term.name,'type':name_var['type']}, ctx)
                typecheck(term.body(name_var), body_typ, defs, show, body_ctx)
            else :
                raise Exception( lambda : ( Err(locs, ctx, 'Lambda has a non-function type.') ) )
        elif case == 'Let':
            expr_typ = typeinfer(term.expr, defs, show, ctx)
            expr_var = Ann(True, Var(term.name+'#'+(ctx.size+1)), expr_typ)
            body_ctx = Ext({'name':term.name,'type':expr_var['type']}, ctx)
            typecheck(term.body(expr_var), type, defs, show, body_ctx)
        elif case == 'Loc':
            locs = {'fr0m': term.fr0m, 'upto': term.upto}
            typecheck(term.expr, type, defs, show, ctx, locs)
        else:
            infr = typeinfer(term, defs, show, ctx)
            eq = equal(type, infr, defs, ctx.size)
            if (not eq) :
                type0_str = show(normalize(type, {}, True), ctx)
                infr0_str = show(normalize(infr, {}, True), ctx)
                raise Exception( lambda : ( Err(locs, ctx,
                    'Found type... \x1b[2m'+infr0_str+'\x1b[0m\n' +
                    'Instead of... \x1b[2m'+type0_str+'\x1b[0m')))
    return {term,type}

def typesynth(name, defs, show = stringify) :
    term = defs[name].term
    type = defs[name].type
    defs[name].core = {term, type}
    return typecheck(term, type, defs, show)

__all__ = (
    'Var',
    'Ref',
    'Typ',
    'All',
    'Lam',
    'App',
    'Let',
    'Ann',
    'Loc',
    'Ext',
    'Nil',
    'find',
    'stringify',
    'parse',
    'reduce',
    'normalize',
    'Err',
    'typeinfer',
    'typecheck',
    'typesynth',
    'equal',
)
