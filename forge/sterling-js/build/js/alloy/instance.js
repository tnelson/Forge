import { Signature } from './signature';
import { Field } from "./field";
import { Skolem } from './skolem';
import { Tuple } from './tuple';
import { Atom } from './atom';
import * as d3 from 'd3';
export class Instance {
    constructor() {
        this.expressionType = () => 'instance';
        this._command = null;
        this._filename = null;
        this._bitwidth = null;
        this._maxseq = null;
        this._builddate = null;
        this._sources = new Map();
        this._signatures = [];
        this._fields = [];
        this._skolem = [];
    }
    atoms() {
        return this.signatures()
            .filter(s => s.label() !== 'seq/Int')
            .map(sig => sig.atoms())
            .reduce((acc, cur) => acc.concat(cur), []);
    }
    bitwidth() {
        return this._bitwidth;
    }
    builddate() {
        return this._builddate;
    }
    command() {
        return this._command;
    }
    fields() {
        return this._fields.slice();
    }
    filename() {
        return this._filename;
    }
    label() {
        return this.toString();
    }
    maxseq() {
        return this._maxseq;
    }
    signatures() {
        return this._signatures.slice();
    }
    skolems() {
        return this._skolem.slice();
    }
    sources() {
        return this._sources;
    }
    tuples() {
        let skolems = this.skolems()
            .filter(skolem => skolem.size() > 1)
            .map(skolem => skolem.tuples())
            .reduce((acc, cur) => acc.concat(cur), []);
        let fields = this.fields()
            .map(fld => fld.tuples())
            .reduce((acc, cur) => acc.concat(cur), []);
        return fields.concat(skolems);
    }
    toString() {
        return 'Instance';
    }
    univ() {
        return this._signatures.find(s => s.label() === 'univ');
    }
    static fromXML(xml) {
        let instance = new Instance(), parser = new DOMParser(), doc = d3.select(parser.parseFromString(xml, "application/xml"));
        let inst = doc.select('instance');
        let ally = doc.select('alloy');
        instance._command = inst.attr('command');
        instance._filename = inst.attr('filename');
        instance._bitwidth = parseInt(inst.attr('bitwidth'));
        instance._maxseq = parseInt(inst.attr('maxseq'));
        instance._builddate = ally.attr('builddate');
        let sigparents = new Map();
        let fldparents = new Map();
        let signatures = new Map();
        let fields = new Map();
        let skolem = new Map();
        // Parse signatures and atoms
        doc.selectAll('sig')
            .filter(subsetTest(false))
            .each(parseSig(instance, sigparents, signatures));
        // Assemble signature hierarchy
        sigparents.forEach((pid, cid) => {
            let parent = signatures.get(pid);
            let child = signatures.get(cid);
            if (parent && child)
                addSignature(parent, child);
        });
        // Parse subsets and atoms
        doc.selectAll('sig')
            .filter(subsetTest(true))
            .each(parseSubset(instance, signatures));
        // Parse fields and tuples
        doc.selectAll('field')
            .each(parseField(fldparents, signatures, fields));
        // Parse skolem
        doc.selectAll('skolem')
            .each(parseSkolem(signatures, skolem));
        // Assemble field hierarchy
        fldparents.forEach((pid, cid) => {
            let parent = signatures.get(pid);
            let child = fields.get(cid);
            if (parent && child)
                addField(parent, child);
        });
        // Save to instance
        instance._signatures = Array.from(signatures.values());
        instance._fields = Array.from(fields.values());
        instance._skolem = Array.from(skolem.values());
        // Save model source
        doc.selectAll('source')
            .each(function () {
            let s = d3.select(this), f = s.attr('filename'), c = s.text();
            instance._sources.set(f, c);
        });
        return instance;
    }
}
function addAtom(sig, label) {
    if (sig._atoms.find(a => a.label() === label))
        throw Error('Atom ' + label + ' already exists in ' + sig);
    sig._atoms.push(new Atom(sig, label));
}
function addField(parent, child) {
    if (child.types()[0] !== parent)
        throw Error('First type of field ' + child + ' must be ' + parent);
    if (parent._fields.find(f => f.label() === child.label()))
        throw Error(parent + ' already contains field ' + child);
    parent._fields.push(child);
    child._parent = parent;
}
function addSignature(parent, child) {
    if (parent._signatures.find(s => s.label() === child.label()))
        throw Error(parent + ' already contains ' + child);
    parent._signatures.push(child);
    child._parent = parent;
}
function addTuple(receiver, tuple) {
    // Array of types for atoms in relation
    let types = receiver.types();
    // Atoms in tuple
    let atoms = tuple.atoms();
    // Check for same number of atoms
    if (atoms.length !== types.length) {
        throw Error(tuple + ' has incorrect number of atoms for ' + receiver);
    }
    // Check that atoms are correct type
    if (!types.every((t, i) => atoms[i].isType(t))) {
        throw Error(tuple + ' incompatible with field ' + receiver);
    }
    // Check that tuple not already in relation
    if (receiver.has(...atoms)) {
        throw Error(receiver + ' already contains ' + tuple);
    }
    tuple._parent = receiver;
    receiver._tuples.push(tuple);
}
function buildInt(sig, bitwidth) {
    if (bitwidth < 1)
        return;
    let n = Math.pow(2, bitwidth);
    for (let i = -n / 2; i < n / 2; ++i) {
        addAtom(sig, i.toString());
    }
}
function parseField(parents, signatures, fields) {
    return function () {
        let f = d3.select(this);
        let id = parseInt(f.attr('ID'));
        let parent = parseInt(f.attr('parentID'));
        let label = f.attr('label');
        let types = f.select('types')
            .selectAll('type')
            .nodes()
            .map(parseType)
            .map(id => signatures.get(id));
        let field = new Field(label, types, f.attr('private') === 'yes', f.attr('meta') === 'yes');
        f.selectAll('tuple')
            .nodes()
            .map(parseTuple)
            .map(t => t.map((a, i) => types[i].atom(a, true)))
            .map(t => new Tuple(t))
            .forEach(tuple => addTuple(field, tuple));
        parents.set(id, parent);
        fields.set(id, field);
    };
}
function parseSubset(instance, signatures) {
    return function () {
        let s = d3.select(this);
        let id = parseInt(s.attr('ID'));
        let types = s.selectAll('type')
            .nodes()
            .map(parseType)
            .map(id => signatures.get(id));
        let sig = new Signature(s.attr('label'), s.attr('builtin') === 'yes', s.attr('private') === 'yes', s.attr('meta') === 'yes', s.attr('one') === 'yes', true);
        signatures.set(id, sig);
        // Sequences aren't explicitly made subset signatures, but in reality
        // that is how they act, so get up to maxseq atoms from int
        if (sig.label() === 'seq/Int') {
            let int = Array
                .from(signatures.values())
                .find(s => s.label() === 'Int');
            for (let i = 0; i < instance.maxseq(); ++i) {
                let label = i.toString();
                let atom = int.atom(label, true);
                sig._atoms.push(atom);
            }
            return;
        }
        s.selectAll('atom')
            .each(function () {
            let label = d3.select(this).attr('label');
            for (let t of types) {
                let atom = t.atom(label, true);
                if (atom) {
                    sig._atoms.push(atom);
                    break;
                }
            }
        });
    };
}
function parseSig(instance, parents, signatures) {
    return function () {
        let s = d3.select(this);
        let id = parseInt(s.attr('ID'));
        let parent = parseInt(s.attr('parentID'));
        let sig = new Signature(s.attr('label'), s.attr('builtin') === 'yes', s.attr('private') === 'yes', s.attr('meta') === 'yes', s.attr('one') === 'yes');
        parents.set(id, parent);
        signatures.set(id, sig);
        s.selectAll('atom')
            .each(function () {
            addAtom(sig, d3.select(this).attr('label'));
        });
        // Integer atoms are not explicitly included, so
        // build them based on the bitwidth
        if (sig.label() === 'Int')
            buildInt(sig, instance.bitwidth());
    };
}
function parseSkolem(signatures, skolems) {
    return function () {
        let s = d3.select(this);
        let id = parseInt(s.attr('ID'));
        let label = s.attr('label');
        let types = s.select('types')
            .selectAll('type')
            .nodes()
            .map(parseType)
            .map(id => signatures.get(id));
        let skolem = new Skolem(label, types);
        s.selectAll('tuple')
            .nodes()
            .map(parseTuple)
            .map(t => t.map((a, i) => types[i].atom(a, true)))
            .map(t => new Tuple(t))
            .forEach(tuple => addTuple(skolem, tuple));
        skolems.set(id, skolem);
    };
}
function parseTuple(tuple) {
    return d3.select(tuple)
        .selectAll('atom')
        .nodes()
        .map((d) => {
        return d3.select(d).attr('label');
    });
}
function parseType(type) {
    return parseInt(d3.select(type).attr('ID'));
}
function subsetTest(keepSubsets) {
    return function () {
        let s = d3.select(this);
        let parentID = s.attr('parentID');
        let label = s.attr('label');
        let issubset = (parentID === null && label !== 'univ') || label === 'seq/Int';
        return keepSubsets ? issubset : !issubset;
    };
}
