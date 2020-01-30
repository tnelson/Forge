import { Atom } from './atom';
import { Field } from './field';
import { Skolem } from './skolem';

export class Tuple {

    expressionType = () => 'tuple';

    _atoms: Array<Atom>;
    _parent: Field | Skolem;

    constructor (atoms: Array<Atom>) {

        this._atoms = atoms;
        this._parent = null;

    }

    atoms (): Array<Atom> {
        return this._atoms.slice();
    }

    id (): string {
        return this._parent
            ? this._parent.id() + '{' + this._atoms.join('->') + '}'
            : this._atoms.join('->');
    }

    parent (): Field | Skolem {
        return this._parent;
    }

    toString (): string {
        return this._atoms.join('->');
    }

}
