import { Signature } from './signature';
import { Field } from './field';
import { Tuple } from './tuple';

export class Atom {

    expressionType = () => 'atom';

    _label: string;
    _signature: Signature;

    constructor (signature: Signature, label: string) {

        this._label = label;
        this._signature = signature;

    }

    id (): string {
        return this._label;
    }

    isType (signature: Signature): boolean {
        return signature === this._signature ||
            this._signature.types().includes(signature);
    }

    join (field: Field): Array<Tuple> {
        return field.tuples()
            .filter(tuple => tuple.atoms().shift() === this)
            .map(tuple => new Tuple(tuple.atoms().slice(1)));
    }

    label (): string {
        return this._label;
    }

    signature (): Signature {
        return this._signature;
    }

    toString (): string {
        return this._label;
    }

}
