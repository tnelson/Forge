export class Skolem {
    constructor(label, types) {
        this.expressionType = () => 'skolem';
        this._label = label;
        this._tuples = [];
        this._types = types;
    }
    has(...atoms) {
        return !!this._tuples.find(t => {
            return t.atoms().every((a, i) => atoms[i] === a);
        });
    }
    id() {
        return this._label;
    }
    label() {
        return this._label;
    }
    size() {
        return this._types.length;
    }
    toString() {
        return this._label;
    }
    tuples() {
        return this._tuples.slice();
    }
    types() {
        return this._types.slice();
    }
}
