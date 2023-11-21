import './style.css'
import { UnifyJS } from 'scalajs:main.js'

window.UnifyJS = UnifyJS
window.Const = UnifyJS.Const
window.Var = UnifyJS.Var
window.Tuple = (...args) => UnifyJS.Tuple(args.map(toTerm))

function toTerm(x) {
    if (typeof x === 'number') return Const(x)
    if (typeof x === 'string') return Var(x)
    if (x instanceof Array) return Tuple(...x.map(toTerm))
}

class And {
    constructor(...ls) {
        this.ls = ls
    }
}

class Or {
    constructor(...ls) {
        this.ls = ls
    }
}

class Fact {
    constructor(name, ...args) {
        this.name = name
        this.args = args
    }
}

class Relation {
    constructor(name, ...disjuncts) {
        this.name = name
        this.disjuncts = disjuncts
    }
}
