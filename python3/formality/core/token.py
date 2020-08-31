"""Formality Core."""
# Translations from the javascript:
#  * adhoc objects replaced with namedtuple classes
#  * functions that switch on the constructor replaced with methods
from collections import namedtuple

Term = namedtuple
List = namedtuple
del namedtuple


class Var(Term("Var", "indx")):
    __slots__ = ()

    def stringify(self, depth=0):
        del depth
        return self.indx.split("#")[0]

    def reduce(self, defs, erased=False):
        del defs, erased
        return self


class Ref(Term("Ref", "name")):
    __slots__ = ()

    def stringify(self, depth=0):
        del depth
        return self.name

    def reduce(self, defs, erased=False) -> Term:
        if defs[self.name]:
            term = defs[self.name].term
            if (  # FIXME: pylint:disable=unidiomatic-typecheck
                type(term) is Loc
                and type(term.expr) is Ref
                and term.expr.name == self.name
            ):
                return term
            else:
                return term.reduce(defs, erased)
        else:
            return Ref(self.name)


class Typ(Term("Typ", "")):
    __slots__ = ()

    @staticmethod
    def stringify(depth=0):
        del depth
        return "*"

    def reduce(self, defs, erased=False):
        del defs, erased
        return self


class All(Term("All", "eras self name bind body")):
    __slots__ = ()

    def stringify(self, depth=0):
        bind = "Ɐ" if self.eras else "Π"
        slf_ = self.self or ("x" + str(depth + 0))
        name = self.name or ("x" + str(depth + 1))
        type = self.bind.stringify(depth)
        body = self.body(Var(slf_ + "#"), Var(name + "#")).stringify(depth + 2)
        return f"{bind}{slf_}({name}:{type}) {body}"

    def reduce(self, defs, erased=False):
        del defs, erased
        return self


def identity(obj):
    return obj


class Lam(Term("Lam", "eras name body")):
    __slots__ = ()

    def stringify(self, depth=0):
        bind = "Λ" if self.eras else "λ"
        name = self.name or ("x" + str(depth))
        body = self.body(Var(name + "#")).stringify(depth)
        return f"{bind}{name} {body}"

    def reduce(self, defs, erased=False):
        if erased and self.eras:
            return self.body(Lam(False, "", identity)).reduce(defs, erased)
        else:
            return self


class App(Term("App", "eras func argm")):
    __slots__ = ()

    def stringify(self, depth=0):
        open = "<" if self.eras else "("
        func = self.func.stringify(depth)
        argm = self.argm.stringify(depth)
        clos = ">" if self.eras else ")"
        return f"{open}{func} {argm}{clos}"

    def reduce(self, defs, erased=False):
        if erased and self.eras:
            return self.func.reduce(defs, erased)
        else:
            func = self.func.reduce(defs, erased)
            if type(func) is Lam:  # FIXME: pylint:disable=unidiomatic-typecheck
                return func.body(self.argm).reduce(defs, erased)
            else:
                return self


class Let(Term("Let", "name expr body")):
    __slots__ = ()

    def stringify(self, depth=0):
        name = self.name or ("x" + str(depth))
        expr = self.expr.stringify(depth)
        body = self.body(Var(name + "#")).stringify(depth + 1)
        return f"${name}={expr};{body}"

    def reduce(self, defs, erased=False):
        term = self.body(self.expr)
        return term.reduce(defs, erased)


class Ann(Term("Ann", "done expr type")):
    __slots__ = ()

    def stringify(self, depth=0):
        type = self.type.stringify(depth)
        expr = self.expr.stringify(depth)
        return f":{type} {expr}"

    def reduce(self, defs, erased=False):
        return self.expr.reduce(defs, erased)


class Loc(Term("Loc", "frum upto expr")):
    __slots__ = ()

    def stringify(self, depth=0):
        return self.expr.stringify(depth)

    def reduce(self, defs, erased=False):
        return self.expr.reduce(defs, erased)


del Term


class Nil(List("Nil", "")):
    __slots__ = ()
    size = 0

    def __new__(cls):
        return super().__new__(cls)

    @staticmethod
    def find(cond, indx=0):
        del cond, indx


class Ext(List("Ext", "head tail size")):
    __slots__ = ()

    def __new__(cls, head, tail):
        super().__new__(cls, head, tail, tail.soze + 1)

    def find(self, cond, indx=0):
        if cond(self.head, indx):
            return self.head, indx
        else:
            return self.tail.find(cond, indx + 1)


del List


__all__ = (
    "Var",
    "Ref",
    "Typ",
    "All",
    "Lam",
    "App",
    "Let",
    "Ann",
    "Loc",
    "Ext",
    "Nil",
    "parse",
    "normalize",
    "Err",
    "typeinfer",
    "typecheck",
    "typesynth",
    "equal",
)
