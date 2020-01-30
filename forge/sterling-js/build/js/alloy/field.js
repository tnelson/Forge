export class Field {
    constructor(label, types, isprivate = false, ismeta = false) {
        this.expressionType = () => 'field';
        this._label = label;
        this._parent = null;
        this._tuples = [];
        this._types = types;
        this._private = isprivate;
        this._meta = ismeta;
    }
    has(...atoms) {
        return !!this._tuples.find(t => {
            return t.atoms().every((a, i) => atoms[i] === a);
        });
    }
    id() {
        return (this._parent ? this._parent + '<:' : '') + this._label;
    }
    label() {
        return this._label;
    }
    meta() {
        return this._meta;
    }
    parent() {
        return this._parent;
    }
    private() {
        return this._private;
    }
    size() {
        return this._types.length;
    }
    toString() {
        return (this._parent ? this._parent + '<:' : '') + this._label;
    }
    tuples() {
        return this._tuples.slice();
    }
    types() {
        return this._types.slice();
    }
}
