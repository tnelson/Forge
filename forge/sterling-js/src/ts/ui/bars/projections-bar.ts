import * as d3 from 'd3';
import { Atom, Instance, Signature } from '../..';

export class ProjectionsBar {

    _projbar;
    _projlist;

    _btn_add;
    _btn_add_items;

    _signatures: Array<Signature>;
    _projections: Map<Signature, Atom>;

    _on_update: Function;

    constructor (selection) {


        this._projbar = selection;
        this._projlist = selection
            .select('#projections-list');

        this._btn_add = selection
            .select('#add-projection')
            .on('click', this._toggle_signatures.bind(this));

        this._btn_add_items = selection
            .selectAll('#add-projection, #add-projection *');

        this._signatures = [];
        this._projections = new Map();

        this._on_update = () => {};

        d3.select('body')
            .on('click', this._on_click.bind(this))

    }

    on_update (callback?: Function) {

        if (!arguments.length) return this._on_update;
        this._on_update = callback;
        return this;

    }

    projections (projections?: Map<Signature, Atom>) {

        if (!arguments.length) return this._projections;
        this._projections = projections;
        return this;

    }

    set_instance (instance: Instance) {

        let projections: Map<Signature, Atom> = new Map();

        let oldsigs = Array.from(this._projections.keys());
        let newsigs = instance.univ().signatures(false);
        let atoms = instance.atoms();

        oldsigs.forEach(signature => {

            let newsig = newsigs.find(sig => sig.id() === signature.id());

            if (newsig) {

                let newatom = atoms.find(atom => atom.id() === this._projections.get(signature).id());

                if (!newatom) {
                    let sigatoms = newsig.atoms(true);
                    if (sigatoms.length) {
                        newatom = sigatoms[0];
                    }
                }

                if (newatom) {
                    projections.set(newsig, newatom);
                }

            }

        });

        this._projections = projections;

        this._set_signatures(instance
            .univ()
            .signatures()
            .filter(sig => {
                return sig.label() !== 'univ' && sig.label() !== 'seq/Int';
            })
            .filter(sig => {
                return !this._projections.has(sig);
            })
            .sort((a, b) => {
                if (a.private() && !b.private()) return 1;
                if (!a.private() && b.private()) return -1;
                return sig_label(a).localeCompare(sig_label(b));
            })
        );

        this._update_projections();

    }

    _add_projection (signature: Signature) {

        this._set_signatures(this._signatures.filter(sig => sig !== signature));

        let atoms = signature.atoms(true);
        this._projections.set(signature, atoms.length ? atoms[0] : null);

        this._update_projections();

    }

    _hide_signatures () {

        this._btn_add
            .selectAll('.dropdown-content')
            .style('display', 'none');

    }

    _on_click () {

        let outside = this._btn_add_items.filter(function () {
            return this === d3.event.target;
        }).empty();

        if (outside) this._hide_signatures();

    }

    _set_projection (atom: Atom, signature: Signature) {

        if (atom === null) {
            this._projections.delete(signature);
        } else {
            this._projections.set(signature, atom);
        }

    }

    _set_signatures (signatures: Array<Signature>) {

        this._signatures = signatures;
        this._update_signatures();

    }

    _trigger_update () {
        this._on_update(this._projections);
    }

    _update_projections () {

        let projections = this._projections;
        let sigs: Array<Signature> = Array.from(projections.keys());
        let set_projection = this._set_projection.bind(this);
        let trigger_update = this._trigger_update.bind(this);
        let add_signature = (signature) => {
            this._signatures.push(signature);
            this._set_signatures(this._signatures);
        };

        this._projlist
            .selectAll('.combo-button')
            .data(sigs, d => d.id())
            .join(enter => add_combo_button(enter))
            .each(function (signature) {

                let projection = d3.select(this);
                let atoms = signature.atoms(true);
                let atom = projections.get(signature);

                let btn_prev = projection.select('#prev');
                let btn_atom = projection.select('#atom');
                let btn_next = projection.select('#next');
                let btn_exit = projection.select('#exit');

                if (!atom) {

                    btn_prev.classed('inactive', true);
                    btn_atom.classed('inactive', true).text(sig_label(signature));
                    btn_next.classed('inactive', true);

                } else {

                    btn_atom.text(atom.label());
                    let i = atoms.indexOf(atom);
                    btn_prev.classed('inactive', i === 0);
                    btn_next.classed('inactive', i === atoms.length-1);
                    set_projection(atom, signature);

                }

                if (atoms.length > 1) {

                    let atomlist = projection.select('#atomlist');

                    atomlist
                        .selectAll('.atom')
                        .data(atoms)
                        .join('div')
                        .attr('class', 'atom dropdown-item')
                        .attr('id', (d: Atom) => d.label())
                        .text((d: Atom) => d.label())
                        .on('click', pick_atom);

                    btn_prev
                        .on('click', () => {
                            let i = atoms.indexOf(atom);
                            if (i > 0) pick_atom(atoms[i-1], i-1);
                        });

                    btn_atom
                        .on('click', () => {
                            let vis = atomlist.style('display');
                            let offset = (btn_prev.node() as any).getBoundingClientRect().width;
                            let width = (btn_atom.node() as any).getBoundingClientRect().width;
                            atomlist
                                .style('display', vis === 'none' ? 'flex' : 'none')
                                .style('left', offset + 'px')
                                .style('min-width', width + 'px');
                        });

                    btn_next
                        .on('click', () => {
                            let i = atoms.indexOf(atom);
                            if (i < atoms.length-1) pick_atom(atoms[i+1], i+1);
                        });

                    function pick_atom (next: Atom, index: number) {
                        atom = next;
                        btn_atom.text(atom.label());
                        atomlist.style('display', 'none');
                        set_projection(atom, signature);
                        btn_prev.classed('inactive', index === 0);
                        btn_next.classed('inactive', index === atoms.length-1);
                        trigger_update();
                    }

                } else {

                    btn_prev.classed('inactive', true);
                    btn_next.classed('inactive', true);

                }

                btn_exit.on('click', () => {
                    set_projection(null, signature);
                    projection.remove();
                    add_signature(signature);
                    trigger_update();
                });

            });

    }

    _update_signatures () {

        this._btn_add
            .select('.dropdown-content')
            .selectAll('.dropdown-item')
            .data(this._signatures, sig => sig.id())
            .join(
                enter => enter.append('div')
                    .attr('class', 'dropdown-item')
                    .text(d => sig_label(d)),
                update => update
                    .text(d => sig_label(d))
            )
            .on('click', d => {
                this._add_projection(d);
                this._trigger_update();
            });

    }

    _toggle_signatures () {

        let curr = this._btn_add
            .select('.dropdown-content')
            .style('display');

        this._btn_add
            .select('.dropdown-content')
            .style('display', curr === 'none' ? 'flex' : 'none');

    }

}

function add_combo_button (enter) {
    let button = enter
        .append('div')
        .attr('class', 'combo-button dropdown');
    button.append('div')
        .attr('class', 'icon')
        .attr('id', 'prev')
        .append('i')
        .attr('class', 'fas fa-chevron-left');
    button.append('div')
        .attr('class', 'text')
        .attr('id', 'atom');
    button.append('div')
        .attr('class', 'icon')
        .attr('id', 'next')
        .append('i')
        .attr('class', 'fas fa-chevron-right');
    button.append('div')
        .attr('class', 'icon separated')
        .attr('id', 'exit')
        .append('i')
        .attr('class', 'fas fa-times');
    button.append('div')
        .attr('class', 'dropdown-content')
        .attr('id', 'atomlist');
    return button;
}

function sig_label (sig: Signature) {
    let label = sig.label();
    return label.substring(0, 5) === 'this/' ? label.substring(5) : label;
}
