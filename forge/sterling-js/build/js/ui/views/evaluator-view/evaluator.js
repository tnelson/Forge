import { EventDispatcher } from '../../../util/event-dispatcher';
export class Evaluator extends EventDispatcher {
    constructor(alloy) {
        super();
        this._alloy = null;
        this._nextid = 0;
        this._pending = null;
        this._alloy = alloy;
        this._alloy.on_eval(this._parse.bind(this));
    }
    evaluate(expression) {
        if (this._pending)
            throw Error('Pending expression result');
        const e = {
            id: this._nextid++,
            expression: expression,
            result: this._alloy.connected() ? null : 'No Connection',
            error: !this._alloy.connected(),
            tuples: []
        };
        this._pending = e;
        this._alloy.request_eval(e.id, e.expression);
    }
    setInstance(instance) {
        instance
            .fields()
            .map(field => toExpression(this._nextid++, field))
            .filter(expression => expression.tuples.length > 0)
            .forEach(expression => this.dispatchEvent({
            type: 'expression',
            expression: expression
        }));
    }
    _parse(response) {
        const pending = this._pending;
        this._pending = null;
        const tokens = response.match(/EVL:(-?\d+):(.*)/);
        if (tokens === null) {
            pending.error = true;
            pending.result = 'Invalid response';
            return;
        }
        const id = parseInt(tokens[1]);
        const result = tokens[2].trim();
        if (id === -1) {
            pending.error = true;
            pending.result = result;
            return;
        }
        if (id !== pending.id) {
            pending.error = true;
            pending.result = 'Response ID mismatch';
        }
        if (result.slice(0, 4) === 'ERR:') {
            pending.result = result.slice(4);
            pending.error = true;
        }
        else if (result === 'true' || result === 'false') {
            pending.result = result === 'true';
            pending.error = false;
        }
        else if (/^-?\d+$/.test(result)) {
            pending.result = parseInt(result);
            pending.error = false;
        }
        else {
            pending.result = result;
            pending.error = false;
            pending.tuples = toTuples(pending.expression, result);
        }
        this.dispatchEvent({
            type: 'expression',
            expression: pending
        });
    }
}
function toExpression(id, field) {
    return {
        id: id,
        expression: field.id(),
        result: null,
        error: false,
        tuples: field.tuples().map(tuple => {
            const atoms = tuple.atoms();
            return {
                source: atoms[0].id(),
                target: atoms[atoms.length - 1].id(),
                middle: atoms.slice(1, atoms.length - 1).map(atom => atom.id()),
                relation: field.id()
            };
        })
    };
}
function toTuples(relation, result) {
    const re = /\{(.*)\}/g;
    if (re.test(result)) {
        if (result === '{}')
            return [];
        if (!result.includes('->'))
            return [];
        const raw_tuples = result.slice(1, -1).split(',');
        return raw_tuples
            .map(tuple => tuple.split('->')
            .map(atom => atom.trim()))
            .map(atoms => {
            const middle = atoms.slice(1, atoms.length - 1);
            return {
                source: atoms[0],
                target: atoms[atoms.length - 1],
                middle: middle,
                relation: relation
            };
        });
    }
    return [];
}
