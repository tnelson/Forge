import { Signature } from './signature';
import { Tuple } from './tuple';
import { Atom } from './atom';

export class Field {

    expressionType = () => 'field';

    _label: string;
    _parent: Signature;
    _tuples: Array<Tuple>;
    _types: Array<Signature>;

    _private: boolean;
    _meta: boolean;

    constructor (label: string,
                 types: Array<Signature>,
                 isprivate: boolean = false,
                 ismeta: boolean = false) {

        this._label = label;
        this._parent = null;
        this._tuples = [];
        this._types = types;
        this._private = isprivate;
        this._meta = ismeta;

    }

    has (...atoms: Array<Atom>): boolean {

        return !!this._tuples.find(t => {
            return t.atoms().every((a, i) => atoms[i] === a);
        });

    }

    id (): string {
        return (this._parent ? this._parent + '<:' : '') + this._label;
    }

    label (): string {
        return this._label;
    }

    meta (): boolean {
        return this._meta;
    }

    parent (): Signature {
        return this._parent;
    }

    private (): boolean {
        return this._private;
    }

    size (): number {
        return this._types.length;
    }

    toString (): string {
        return (this._parent ? this._parent + '<:' : '') + this._label;
    }

    tuples (): Array<Tuple> {
        return this._tuples.slice();
    }

    types (): Array<Signature> {
        return this._types.slice();
    }

}
