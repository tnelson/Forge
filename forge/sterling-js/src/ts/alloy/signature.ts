import { Atom } from './atom';
import { Field } from './field';

export class Signature {

    expressionType = () => 'signature';

    _label: string;
    _parent: Signature;

    _builtin: boolean;
    _private: boolean;
    _meta: boolean;
    _one: boolean;
    _subset: boolean;

    _atoms: Array<Atom>;
    _fields: Array<Field>;
    _signatures: Array<Signature>;
    _maxseq: number;

    constructor (label: string,
                 isbuiltin: boolean = false,
                 isprivate: boolean = false,
                 ismeta: boolean = false,
                 isone: boolean = false,
                 issubset: boolean = false) {

        this._label = label;
        this._parent = null;

        this._builtin = isbuiltin;
        this._private = isprivate;
        this._meta = ismeta;
        this._one = isone;
        this._subset = issubset;

        this._atoms = new Array<Atom>();
        this._fields = new Array<Field>();
        this._signatures = new Array<Signature>();

    }

    atom (label: string, nest: boolean = false): Atom {

        return this.atoms(nest).find(a => a.label() === label);
    }

    atoms (nest: boolean = false): Array<Atom> {

        if (!nest) return this._atoms.slice();

        return this.atoms()
            .concat(
                this.signatures(true)
                    .reduce((acc, cur) => acc.concat(cur.atoms()), [])
            );

    }

    builtin (): boolean {
        return this._builtin;
    }

    fields (): Array<Field> {
        return this._fields.slice();
    }

    id (): string {
        return this._label;
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

    signature (label: string, nest: boolean = false) {

        return this.signatures(nest).find(s => s.label() === label);

    }

    signatures (nest: boolean = false): Array<Signature> {

        if (!nest) return this._signatures.slice();

        return this.signatures()
            .concat(
                this._signatures.map(s => s.signatures(true))
                    .reduce((acc, cur) => acc.concat(cur), [])
            );
    }

    toString (): string {
        return this._label;
    }

    types (): Array<Signature> {

        let hierarchy = this._parent ? this._parent.types() : [];
        if (this._label !== 'univ') hierarchy.push(this);
        return hierarchy;

    }

}
