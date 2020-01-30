export class Tuple {
    constructor(atoms) {
        this.expressionType = () => 'tuple';
        this._atoms = atoms;
        this._parent = null;
    }
    atoms() {
        return this._atoms.slice();
    }
    id() {
        return this._parent
            ? this._parent.id() + '{' + this._atoms.join('->') + '}'
            : this._atoms.join('->');
    }
    parent() {
        return this._parent;
    }
    toString() {
        return this._atoms.join('->');
    }
}
