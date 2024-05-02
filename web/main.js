import './style.css'
import relationsStr from './relations.dl?raw'
import factsStr from './facts.dl?raw'
import { UnifyJS } from 'scalajs:main.js'

window.UnifyJS = UnifyJS;
window.Const = UnifyJS.Const;
window.Var = UnifyJS.Var;
window.Tuple = (...args) => {
    let xs = [...args.map(toTerm)];
    return UnifyJS.Tuple(xs);
}

window.Eq = (a, b) => UnifyJS.Eq(toTerm(a), toTerm(b));
window.And = (...args) => args.reduce((a, b) => UnifyJS.And(a, b));
window.Or = (...args) => args.reduce((a, b) => UnifyJS.Or(a, b));
window.Not = UnifyJS.Not;
window.Fact = (name, ...args) => {
    let arg_terms = [...args.map(toTerm)];
    return UnifyJS.Fact(name, arg_terms);
}
window.RelApp = (name, ...args) => {
    let arg_terms = [...args.map(toTerm)];
    return UnifyJS.RelApp(name, arg_terms);
}

window.Relation = (args, formula) => UnifyJS.Relation([...args.map(x => x.trim())], formula);

window.solve = (f, facts, relations) => UnifyJS.solve(f, facts, relations);

window.mkRel = name => (...args) => window.RelApp(name, ...args.map(toTerm));
window.mkFact = name => (...args) => window.Fact(name, ...args.map(toTerm));

window.ConsList = (...els) => els.reduceRight((a, b) => Tuple(toTerm(b), toTerm(a)), []);

function toTerm(x) {
    if (typeof x === 'number') return Const(x);
    if (typeof x === 'string') return Var(x);
    if (x instanceof Array) return Tuple(...x.map(toTerm));
    if (x.__proto__.$classData.name.startsWith('scalogic.reunify.Term')) return x;
}
window.toTerm = toTerm;

document.addEventListener('alpine:init', () => {
    Alpine.data('env', () => ({
        variables: 'x, y, z, xh, xs, yh, ys, a, b, c, ls, l1, l2, yh, yp',
        facts: factsStr,
        relations: relationsStr,
        example_queries: {
            "Generate list of length": 'sameLength(ConsList(1, 3, 5, 9), a)',
            "Edge": 'edge(1, 3)',
            "Connection": 'connected(1, 3)',
            "Check contains": 'contains(ConsList(1, 3, 5), 3)',
            "Generate list containing": 'containsAll(ConsList(1, 3, 5, 9), [3, z])',
            "reverse": "reverseOf(ConsList(1, 3, 5, 9), a)",
            "fancy palindrome": "And(palindrome(x), containsAll(ConsList(1, 3, 5, 9), x), Eq(x, [2, xs]))",
            "cycle": "And(Eq(x, [y]), Eq(y, [z]), Eq(z, [x]))"
        },
        query: 'sameLength(ConsList(1, 3, 5, 9), a)',
        query_key: 'Generate list of length',
        result: 'None (yet)',
        num_results: 0,
        time: undefined,

        get_vars() {
            return this.variables.split(',').map(x => x.trim()).filter(x => x != '');
        },

        get_fact_names() {
            // return this.fact_names.split(',').map(x => x.trim()).filter(x => x != '');
            return this.get_facts().map(f => f.split('(')[0]);
        },

        get_facts() {
            return this.facts.split(';').map(x => x.trim()).filter(x => x != '');
        },

        get_relation_names() {
            // return this.relation_names.split(',').map(x => x.trim()).filter(x => x != '');
            return this.get_relations().map(r => r.split('(')[0]);
        },

        get_relations() {
            return this.relations.split(';').map(x => x.trim()).filter(x => x != '');
        },

        solve() {
            UnifyJS.clearFreshVarCounts();
            try {
                for (let v of this.get_vars()) {
                    eval(`window.${v} = Var('${v}')`);
                }

                for (let f of this.get_fact_names()) {
                    eval(`window.${f} = mkFact('${f}')`);
                }

                for (let r of this.get_relation_names()) {
                    eval(`window.${r} = mkRel('${r}')`);
                }

                let facts = this.get_facts().map(f => {
                    let [name, args] = f.slice(0, -1).split('(');
                    let arg_ls = args.split(',').filter(x => x != '').map(eval);
                    let arg_terms = arg_ls.map(toTerm);
                    return Fact(name, ...arg_terms);
                });

                let relations = this.get_relations().map(r => {
                    let [decl, body] = r.trim().split(':-');
                    let [name, arg_str] = decl.trim().slice(0, -1).split('(');
                    let args = arg_str.split(',').filter(x => x != '');
                    return { name, args, body };
                });

                let relMap = new Map();
                for (let rel of relations) {
                    relMap.set(rel.name, Relation(rel.args, eval(rel.body)));
                }

                let query = eval(this.query);
                console.log(query, facts, relMap);

                let t0 = Date.now();
                let res = solve(query, facts, relMap);
                let t1 = Date.now();
                this.result = res.display();
                this.num_results = res.length();
                this.time = (t1 - t0) / 1000;
            } catch (e) {
                this.result = e.toString();
            }
        },

        init_fact_editor(el, update) {
            let editor = ace.edit(el);
            editor.setValue(this.facts, -1);
            editor.session.on('change', () => this.facts = editor.getValue());
        },

        init_rel_editor(el) {
            let editor = ace.edit(el);
            editor.setValue(this.relations, -1);
            editor.session.on('change', () => this.relations = editor.getValue());
        },
    }));
});
