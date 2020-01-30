import * as d3 from 'd3';
import { Atom, Instance, Signature } from '..';

export class AlloyGraph {

    // The instance represented by this graph
    _instance: Instance;

    // Map of projections
    _projections: Map<Signature, Atom>;

    // Flags determine if certain types of expression are filtered out of graph
    _builtin: boolean = true;
    _disconnected: boolean = true;
    _meta: boolean = true;
    _private: boolean = true;

    constructor (instance: Instance) {

        this._instance = instance;
        this._projections = new Map();

    }

    filter_builtins (filter?: boolean) {
        if (!arguments.length) return this._builtin;
        this._builtin = filter;
        return this;
    }

    filter_disconnected (filter?: boolean) {
        if (!arguments.length) return this._disconnected;
        this._disconnected = filter;
        return this;
    }

    filter_meta (filter?: boolean) {
        if (!arguments.length) return this._meta;
        this._meta = filter;
        return this;
    }

    filter_private (filter?: boolean) {
        if (!arguments.length) return this._private;
        this._private = filter;
        return this;
    }

    graph () {

        // Build a tree containing signatures and atoms as nodes
        let tree = d3.hierarchy(this._instance.univ(), d => {
            if (d.expressionType() === 'signature')
                return (d.signatures() as any).concat(d.atoms());
        });

        // Build all edges by getting all tuples and projecting
        let edges = this._instance
            .tuples()
            .map(tuple => {

                let atoms = tuple.atoms();

                this._projections.forEach((atom, signature) => {
                    atoms = project(atoms, atom, signature)
                });

                return {
                    data: tuple,
                    source: atoms.length >= 2 ? atoms[0] : null,
                    target: atoms.length >= 2 ? atoms[atoms.length-1] : null,
                    middle: atoms.length > 2 ? atoms.slice(1, atoms.length-1) : []
                };

            })
            .filter(edge => edge.source !== null && edge.target !== null)
            .filter(edge => !this._builtin || !edge_is_builtin(edge))
            .filter(edge => !this._private || !edge_is_private(edge));

        // Determine the set of all nodes used in a relation
        let nodeset = new Set();
        edges.forEach(edge => edge.data.atoms().forEach(atom => nodeset.add(atom.id())));

        // Determine the set of visible nodes
        let visibleset = new Set();
        edges.forEach(edge => visibleset.add(edge.source.id()).add(edge.target.id()));

        // Remove atoms from tree based on flags
        tree.eachAfter(node => {

            if (node.data.expressionType() === 'signature') {

                // Keep a complete copy of children
                (node as any)._children = node.children;

                let signature: Signature = node.data;
                let hide =
                    (this._builtin && signature.builtin()) ||
                    (this._meta && signature.meta()) ||
                    (this._private && signature.private());

                if (hide && node.children) {
                    node.children = node.children.filter(child => {

                        // If a child node is a signature, we always want to
                        // include it.  If not, it is an atom and we only want
                        // to include it if it is used in a relation
                        return child.data.expressionType() === 'signature'
                            || nodeset.has(child.data.id());

                    });

                }

                // (Optionally) Remove atoms that are not part of a relation
                // if (this._disconnected && node.children) {

                //     node.children = node.children.filter(child => {

                //         // If a child node is a signature, we always want to
                //         // include it.  If not, it is an atom and we only want
                //         // to include it if it is visible as part of an edge
                //         return child.data.expressionType() === 'signature'
                //             || visibleset.has(child.data.id());
                //     });

                // }

                // Remove atoms that are part of a projected signature
                if (node.children) {

                    let sigs = Array.from(this._projections.keys());

                    node.children = node.children.filter(child => {

                        return child.data.expressionType() === 'signature'
                            || !sigs.includes((child.data as any).signature());

                    });

                }

                // Remove integers that are not part of a relation
                if (node.children) {

                    node.children = node.children.filter(child => {

                        let isint = child.data.expressionType() === 'atom'
                            && (child.data as any).signature().label() === 'Int';

                        return !isint || visibleset.has(child.data.id());

                    });

                }

            }

        });

        // Remove signatures that have no children
        tree.eachAfter(node => {

            if (node.children) {

                node.children = node.children.filter(child => {

                    return child.data.expressionType() === 'atom'
                        || (child.children && child.children.length);

                });

            }

        });

        return {
            tree,
            edges
        };

    }

    project (atom: Atom) {

        // Determine top level signature of this atom
        let types = atom.signature().types();

        if (!types.length)
            throw Error(atom + ' has no type');

        let signature = types[0];

        this._projections.set(signature, atom);

    }

    projections (projections: Map<Signature, Atom>) {
        if (!arguments.length) return this._projections;
        this._projections = projections;
        return this;
    }

    unproject (atom: Atom) {

        // Determine top level signature of this atom
        let types = atom.signature().types();

        if (!types.length)
            throw Error(atom + ' has no type');

        let signature = types[0];

        this._projections.delete(signature);

    }

}

function edge_is_builtin (edge) {

    let sourcesig = edge.source.signature(),
        targetsig = edge.target.signature();

    return (sourcesig.label() !== "Int" && sourcesig.builtin())
        || (targetsig.label() !== "Int" && targetsig.builtin());

}

function edge_is_private (edge) {

    return edge.source.signature().private() || edge.target.signature().private();

}

function project (tup: Array<Atom>, atom: Atom, signature: Signature): Array<Atom> {

    if (tup.includes(atom)) {

        return tup.filter(a => a !== atom);

    } else {

        // If the tuple does not contain an atom of type signature, it
        // remains unchanged, otherwise the entire tuple is removed
        let hastype = tup.reduce((acc, cur) => acc || cur.isType(signature), false);

        if (hastype) {
            return [];
        } else {
            return tup;
        }

    }

}
