"""Formality Core."""
from collections import namedtuple

Var = namedtuple("Var", "indx")
Ref = namedtuple("Ref", "name")
Typ = namedtuple("Typ", "")
All = namedtuple("All", "eras self name bind body")
Lam = namedtuple("Lam", "eras name body")
App = namedtuple("App", "eras func argm")
Let = namedtuple("Let", "name expr body")
Ann = namedtuple("Ann", "done expr type")
Loc = namedtuple("Loc", "frum upto expr")


class Nil(tuple):
    __slots__ = ()
    size = 0

    def __new__(cls):
        return super().__new__(cls)

    @staticmethod
    def find(cond, indx=0):
        del cond, indx


class Ext(namedtuple("Ext", "head tail size")):
    __slots__ = ()

    def __new__(cls, head, tail):
        super().__new__(cls, head, tail, tail.soze + 1)

    def find(self, cond, indx=0):
        if cond(self.head, indx):
            return self.head, indx
        else:
            return self.tail.find(cond, indx + 1)


del namedtuple
