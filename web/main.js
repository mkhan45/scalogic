import './style.css'
import { UnifyJS } from 'scalajs:main.js'

window.UnifyJS = UnifyJS;
window.Const = UnifyJS.Const;
window.Var = UnifyJS.Var;
window.Tuple = (...args) => UnifyJS.Tuple(args.map(toTerm));

window.Eq = (a, b) => UnifyJS.Eq(toTerm(a), toTerm(b));
window.And = (...args) => args.reduce((a, b) => UnifyJS.And(a, b));
window.Or = (...args) => args.reduce((a, b) => UnifyJS.Or(a, b));
window.Not = UnifyJS.Not;
window.Fact = (name, ...args) => UnifyJS.Fact(name, args.map(toTerm));
window.RelApp = (name, ...args) => UnifyJS.RelApp(name, args.map(toTerm));

window.Relation = (args, formula) => UnifyJS.Relation(args, formula);

window.solve = (f, facts, relations) => UnifyJS.solve(f, facts, relations);

function toTerm(x) {
    if (typeof x === 'number') return Const(x)
    if (typeof x === 'string') return Var(x)
    if (x instanceof Array) return Tuple(...x.map(toTerm))
    if (x.__proto__.$classData.name.startsWith('scalogic.unify.Term')) return x;
}
window.toTerm = toTerm;

// class And {
//     constructor(...ls) {
//         this.ls = ls
//     }
// }

// class Or {
//     constructor(...ls) {
//         this.ls = ls
//     }
// }

// class Fact {
//     constructor(name, ...args) {
//         this.name = name
//         this.args = args
//     }
// }

// class Relation {
//     constructor(name, ...disjuncts) {
//         this.name = name
//         this.disjuncts = disjuncts
//     }
// }

document.addEventListener('alpine:init', () => {

    Alpine.data('db', () => ({
        variables: 'x, y, z',
        facts: ['edge(1, 2)', 'edge(2, 3)'],
        relations: ["connected(x, z) :- Or(edge(x, z), And(edge(x, y), connected(y, z)))"],
        query: 'connected(1, 3)',
        result: 'None',

        parse_fact(fact) {
            try {
                let [name, args] = fact.slice(0, -1).split('(');
                let arg_ls = args.split(',').filter(x => x != '').map(eval);
                return { name, args: arg_ls.map(toTerm) };
            } catch (e) {
                console.log(e)
                return false;
            }
        },

        invalid_fact(fact) {
            return this.parse_fact(fact) === false;
        },

        add_fact() {
            this.facts.push('fact()')
        },

        remove_fact(i) {
            this.facts = this.facts.splice(i, i);
        },

        eval_vars() {
            let vars = this.variables.split(',').map(x => x.trim());
            for (let v of vars) {
                eval(`window.${v} = Var('${v}')`);
            }
        },

        parse_relation(rel) {
            try {
                this.eval_vars();
                let [decl, body] = rel.trim().split(':-');
                let [name, args] = decl.trim().slice(0, -1).split('(');
                let arg_ls = args.split(',').filter(x => x != '').map(eval);
                return { name, args: arg_ls.map(toTerm), body };
            } catch (e) {
                console.log(e);
                return false;
            }
        },

        invalid_rel(rel) {
            return this.parse_relation(rel) === false;
        },

        add_rel() {
            this.relations.push('rel() :- Eq(1, 1)')
        },

        remove_rel(i) {
            this.relations = this.relations.splice(i, i);
        },

        run_query() {
            this.eval_vars();

            let facts = this.facts.map(this.parse_fact);
            console.log(facts);
            for (let fact of facts) {
                eval(`window.${fact.name} = (...args) => window.Fact('${fact.name}', ...args)`);
            }

            let relations = this.relations.map(r => this.parse_relation(r));
            console.log(this.relations, relations);

            for (let rel of relations) {
                eval(`window.${rel.name} = (...args) => window.RelApp('${rel.name}', ...args)`);
            }

            let query = eval(this.query);
            console.log(query);

            let scala_facts = facts.map(fact => window.Fact(fact.name, ...fact.args));
            let scala_relations = new Map();
            for (let rel of relations) {
                let scala_relation = window.Relation(rel.args, eval(rel.body));
                scala_relations.set(rel.name, scala_relation);
            }
            console.log(query, scala_facts, scala_relations);
            let result = window.solve(query, scala_facts, scala_relations);
            console.log(result);
            this.result = result.display();
        }
    }))

});
