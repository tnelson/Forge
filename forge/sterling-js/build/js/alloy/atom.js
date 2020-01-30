import { Tuple } from './tuple';
export class Atom {
    constructor(signature, label) {
        this.expressionType = () => 'atom';
        this._label = label;
        this._signature = signature;
    }
    id() {
        return this._label;
    }
    isType(signature) {
        return signature === this._signature ||
            this._signature.types().includes(signature);
    }
    join(field) {
        return field.tuples()
            .filter(tuple => tuple.atoms().shift() === this)
            .map(tuple => new Tuple(tuple.atoms().slice(1)));
    }
    label() {
        return this._label;
    }
    signature() {
        return this._signature;
    }
    toString() {
        return this._label;
    }
}
