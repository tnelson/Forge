export class Signature {
    constructor(label, isbuiltin = false, isprivate = false, ismeta = false, isone = false, issubset = false) {
        this.expressionType = () => 'signature';
        this._label = label;
        this._parent = null;
        this._builtin = isbuiltin;
        this._private = isprivate;
        this._meta = ismeta;
        this._one = isone;
        this._subset = issubset;
        this._atoms = new Array();
        this._fields = new Array();
        this._signatures = new Array();
    }
    atom(label, nest = false) {
        return this.atoms(nest).find(a => a.label() === label);
    }
    atoms(nest = false) {
        if (!nest)
            return this._atoms.slice();
        return this.atoms()
            .concat(this.signatures(true)
            .reduce((acc, cur) => acc.concat(cur.atoms()), []));
    }
    builtin() {
        return this._builtin;
    }
    fields() {
        return this._fields.slice();
    }
    id() {
        return this._label;
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
    signature(label, nest = false) {
        return this.signatures(nest).find(s => s.label() === label);
    }
    signatures(nest = false) {
        if (!nest)
            return this._signatures.slice();
        return this.signatures()
            .concat(this._signatures.map(s => s.signatures(true))
            .reduce((acc, cur) => acc.concat(cur), []));
    }
    toString() {
        return this._label;
    }
    types() {
        let hierarchy = this._parent ? this._parent.types() : [];
        if (this._label !== 'univ')
            hierarchy.push(this);
        return hierarchy;
    }
}
