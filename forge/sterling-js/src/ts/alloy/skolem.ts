import { Tuple } from './tuple';
import { Signature } from './signature';
import { Atom } from './atom';

export class Skolem {

    expressionType = () => 'skolem';

    _label: string;
    _tuples: Array<Tuple>;
    _types: Array<Signature>;

    constructor (label: string, types: Array<Signature>) {

        this._label = label;
        this._tuples = [];
        this._types = types;

    }

    has (...atoms: Array<Atom>): boolean {

        return !!this._tuples.find(t => {
            return t.atoms().every((a, i) => atoms[i] === a);
        });

    }

    id (): string {
        return this._label;
    }

    label (): string {
        return this._label;
    }

    size (): number {
        return this._types.length;
    }

    toString (): string {
        return this._label;
    }

    tuples (): Array<Tuple> {
        return this._tuples.slice();
    }

    types (): Array<Signature> {
        return this._types.slice();
    }

}
