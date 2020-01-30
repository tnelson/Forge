import * as d3 from 'd3';
import { TreeLayoutPreferences } from './tree-layout-preferences';
export class TreeLayout {
    constructor(svg, preferences) {
        this._prefs = preferences ? preferences : new TreeLayoutPreferences();
        this._data = new Map();
        // Initialize svg
        this._svg = svg
            .style('user-select', 'none')
            .style('cursor', 'grab');
        // Initialize link group
        this._gLink = svg
            .append('g')
            .attr('fill', 'none');
        // Initialize node group
        this._gNode = svg
            .append('g')
            .attr('cursor', 'pointer')
            .attr('pointer-events', 'all');
        // Apply general attributes
        apply_attributes(this._svg, this._prefs.font_attributes());
        apply_attributes(this._gLink, this._prefs.link_stroke_attributes());
        this._instance = null;
        this._root = null;
        this._tree = d3.tree();
        this._linkHorizontal = d3.linkHorizontal()
            .x(d => d.y)
            .y(d => d.x);
        this.resize();
        this._extrasep = 0;
        let zoom = d3.zoom()
            .filter(() => {
            return !(d3.event.type === 'wheel' && d3.event.shiftKey);
        })
            .on('zoom', () => {
            this._gLink.attr('transform', d3.event.transform);
            this._gNode.attr('transform', d3.event.transform);
        })
            .on('start', () => this._svg.style('cursor', 'grabbing'))
            .on('end', () => this._svg.style('cursor', 'grab'));
        this._svg
            .call(zoom)
            .on('wheel', () => {
            if (d3.event.shiftKey) {
                this._extrasep += d3.event.deltaY;
                this.redraw();
            }
        });
    }
    redraw() {
        if (this._root)
            this._update(this._root);
    }
    resize() {
        this._width = parseInt(this._svg.style('width'));
        this._height = parseInt(this._svg.style('height'));
        this._svg
            .attr('viewBox', [0, -this._height / 2, this._width, this._height]);
        this.redraw();
    }
    set_instance(instance) {
        let root = to_hierarchy(instance, this._prefs);
        this._set_root(root);
        this._update(this._instance ? null : root);
        this._instance = instance;
    }
    visible_depth(node, max) {
        node = node ? node : this._root;
        max = max ? max : 0;
        return node.children
            ? Math.max(...node.children.map(c => this.visible_depth(c, max + 1)))
            : max;
    }
    _set_root(root) {
        root.descendants().forEach(d => {
            // Store children for collapsing
            d._children = d.children;
            // Determine if this datum is in the existing dataset
            // and if it is, copy its collapsed state
            let id = unique_id(d);
            let datum = this._data.get(id);
            if (datum) {
                d.children = datum.collapsed ? null : d.children;
            }
        });
        this._root = root;
    }
    _update(source) {
        // Update node size based on margins and tree depth
        let hmargin = this._prefs.margin.left + this._prefs.margin.right;
        let theight = this._root.height;
        let nodewidth = this._prefs.font_size;
        let nodeheight = ((this._width - hmargin) / theight) - this._extrasep;
        this._tree.nodeSize([nodewidth, nodeheight]);
        // Update the full tree layout
        this._tree(this._root);
        // Offset position of each node by margins
        this._root.each(d => {
            d.x += this._prefs.margin.top;
            d.y += this._prefs.margin.left;
        });
        // Get the set of nodes and links
        let nodes = this._root.descendants();
        let links = this._root.links();
        // Create the transition
        let transition = this._svg
            .transition()
            .duration(this._prefs.transition_duration);
        // Join node data to elements
        let node = this._gNode
            .selectAll('g')
            .data(nodes, unique_id);
        let enterNode = this._enter_nodes(node.enter(), source);
        this._update_nodes(node.merge(enterNode), transition);
        this._exit_nodes(node.exit(), transition, source);
        // Join link data to elements
        let link = this._gLink
            .selectAll('path')
            .data(links, d => unique_id(d.target));
        let enterLink = this._enter_links(link.enter(), source);
        this._update_links(link.merge(enterLink), transition);
        this._exit_links(link.exit(), transition, source);
    }
    _enter_links(selection, source) {
        let starting_position = d => {
            if (source) {
                let id = unique_id(source);
                let datum = this._data.get(id);
                let pos = { x: datum._x, y: datum._y };
                return this._linkHorizontal({ source: pos, target: pos });
            }
            else {
                let ancestor = this._highest_visible_ancestor(d.target);
                let id = unique_id(ancestor);
                let datum = this._data.get(id);
                return this._linkHorizontal({ source: datum, target: datum });
            }
        };
        return selection
            .append('path')
            .attr('id', d => unique_id(d.target))
            .attr('d', starting_position);
    }
    _enter_nodes(selection, source) {
        // Add all entering nodes to the existing data
        selection.each(d => {
            let id = unique_id(d);
            if (!this._data.has(id)) {
                this._data.set(unique_id(d), {
                    collapsed: false,
                    x: d.x,
                    y: d.y
                });
            }
        });
        // If a source is provided, add nodes at its old position.
        // Otherwise, add node at location of highest visible ancestor.
        let starting_position = d => {
            if (source) {
                let id = unique_id(source);
                return translate(this._data.get(id));
            }
            else {
                let ancestor = this._highest_visible_ancestor(d);
                let id = unique_id(ancestor);
                return translate(this._data.get(id));
            }
        };
        let toggle = d => {
            let id = unique_id(d);
            let info = this._data.get(id);
            info.collapsed = !info.collapsed;
            d.children = d.children ? null : d._children;
            this._update(d);
        };
        let label = d => {
            return d.data.expressionType() === 'tuple'
                ? d.data.atoms().join(' &rarr; ')
                : d.data.label();
        };
        let enter = selection
            .append('g')
            .attr('id', unique_id)
            .attr('class', d => d.data.expressionType())
            .attr('transform', starting_position)
            .attr('fill-opacity', 0)
            .attr('stroke-opacity', 0)
            .on('click', toggle);
        let circle = enter.append('circle');
        let text = enter.append('text').html(label);
        let lower = text.clone(true).attr('class', 'lower').lower();
        apply_attributes(circle, this._prefs.node_attributes());
        apply_attributes(text, this._prefs.text_attributes());
        apply_attributes(lower, this._prefs.text_lower_attributes());
        return enter;
    }
    _update_links(selection, transition) {
        return selection
            .transition(transition)
            .attr('d', this._linkHorizontal);
    }
    _update_nodes(selection, transition) {
        let data = this._data;
        let sep = this._prefs.node_text_separation;
        selection.each(function (d) {
            let id = unique_id(d);
            let info = data.get(id);
            info._x = info.x;
            info._y = info.y;
            info.x = d.x;
            info.y = d.y;
            d3.select(this)
                .selectAll('text')
                .transition(transition)
                .attr('x', function () {
                return d._children
                    ? -this.getBBox().width - sep
                    : sep;
            });
        });
        return selection
            .transition(transition)
            .attr('transform', translate)
            .attr('fill-opacity', 1)
            .attr('stroke-opacity', 1);
    }
    _exit_links(selection, transition, source) {
        let ending_position = d => {
            if (source) {
                let id = unique_id(source);
                let datum = this._data.get(id);
                return this._linkHorizontal({ source: datum, target: datum });
            }
            else {
                let ancestor = this._highest_visible_ancestor(d.target);
                let id = unique_id(ancestor);
                let datum = this._data.get(id);
                return this._linkHorizontal({ source: datum, target: datum });
            }
        };
        return selection
            .transition(transition)
            .remove()
            .attr('d', ending_position);
    }
    _exit_nodes(selection, transition, source) {
        // Remove all exiting nodes from existing data
        if (!source)
            selection.each(d => this._data.delete(unique_id(d)));
        // If a source is provided, transition to that position before removing.
        // Otherwise, transition to location of highest visible ancestor.
        let ending_position = d => {
            if (source) {
                return translate(source);
            }
            else {
                let ancestor = this._highest_visible_ancestor(d);
                let id = unique_id(ancestor);
                return translate(this._data.get(id));
            }
        };
        return selection
            .transition(transition)
            .remove()
            .attr('transform', ending_position)
            .attr('fill-opacity', 0)
            .attr('stroke-opacity', 0);
    }
    _highest_visible_ancestor(d) {
        let current = null;
        let data = this._data;
        function next(datum) {
            let parent = datum.parent;
            if (!parent) {
                current = current || d;
                return;
            }
            let id = unique_id(parent);
            let info = data.get(id);
            if (info) {
                current = info.collapsed // is the parent collapsed?
                    ? null // yes, so null out current
                    : current // no, is there a current?
                        ? current // yes, so that remains the current
                        : parent; // no, so parent is the new current
            }
            next(parent);
        }
        next(d);
        return current;
    }
}
function apply_attributes(selection, attributes) {
    for (let attribute in attributes) {
        selection.attr(attribute, attributes[attribute]);
    }
}
function translate(d) {
    return `translate(${d.y},${d.x})`;
}
function to_hierarchy(instance, p) {
    return d3.hierarchy(instance, function (d) {
        let type = d.expressionType();
        if (type === 'instance') {
            let arr = [];
            return arr
                .concat(d.univ())
                .concat(d.skolems());
        }
        if (type !== 'tuple' && d.label() === 'univ')
            return d.signatures()
                .filter(s => p.show_builtins ? true : !s.builtin());
        if (type === 'signature')
            return d.atoms();
        if (type === 'atom') {
            let fields = d.signature().fields();
            fields.forEach((field) => field.atom = d);
            return fields;
        }
        if (type === 'field')
            return d.atom.join(d);
        if (type === 'skolem')
            return d.tuples();
    });
}
function unique_id(d) {
    return d.parent
        ? unique_id(d.parent) + '.' + d.data.toString()
        : d.data.toString();
}
