import './style.css'
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

function toTerm(x) {
    if (typeof x === 'number') return Const(x);
    if (typeof x === 'string') return Var(x);
    if (x instanceof Array) return Tuple(...x.map(toTerm));
    if (x.__proto__.$classData.name.startsWith('scalogic.unify.Term')) return x;
}
window.toTerm = toTerm;

document.addEventListener('alpine:init', () => {
    Alpine.data('env', () => ({
        variables: 'x, y, z, xh, xs, yh, ys, a, b, c',
        fact_names: 'edge',
        facts: 'edge(1, 2)\nedge(2, 3)',
        relation_names: 'connected, sameLength',
        relations: 'connected(x, z) :- Or(edge(x, z), And(edge(x, y), connected(y, z)));\n' +
                   'sameLength(x, y) :- Or(\n' +
                   '  And(Eq(x, []), Eq(y, [])),\n' +
                   '  And(\n' +
                   '    Eq(x, [xh, xs]), Eq(y, [yh, ys]),\n' +
                   '    sameLength(xs, ys)\n' +
                   '  )\n' +
                   ')',
        query: 'sameLength([1, [3, [5, []]]], a)',
        result: 'None (yet)',

        get_vars() {
            return this.variables.split(',').map(x => x.trim()).filter(x => x != '');
        },

        get_fact_names() {
            return this.fact_names.split(',').map(x => x.trim()).filter(x => x != '');
        },

        get_facts() {
            return this.facts.split('\n').map(x => x.trim()).filter(x => x != '');
        },

        get_relation_names() {
            return this.relation_names.split(',').map(x => x.trim()).filter(x => x != '');
        },

        get_relations() {
            return this.relations.split(';').map(x => x.trim()).filter(x => x != '');
        },

        solve() {
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

            try {
                this.result = solve(query, facts, relMap).display();
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
