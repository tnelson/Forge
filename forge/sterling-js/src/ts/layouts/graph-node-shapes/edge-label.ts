interface EdgeLabelFunction {
    attr: Function,
    style: Function,
    selector: Function,
    transition: Function,
    exit: Function
}

export function edge_label (): EdgeLabelFunction {

    let _selection,
        _selector = 'text',
        _attributes = new Map(),
        _styles = new Map(),
        _transition = null;

    _attributes
        .set('dy', '0.31em')
        .set('text-anchor', 'middle')
        .set('text-rendering', 'geometricPrecision')
        .set('x', d => d.x)
        .set('y', d => d.y);

    _styles
        .set('fill', 'black')
        .set('fill-opacity', 1)
        .set('font-size', '12px')
        .set('font-weight', 'regular')
        .set('stroke', 'none')
        .set('stroke-opacity', 1);

    function _function (selection) {

        _selection = selection
            .selectAll(_selector)
            .data(d => [d])
            .join(
                enter => enter.append('text')
                    .call(apply_attributes)
                    .call(apply_styles)
                    .style('fill-opacity', 0)
                    .style('stroke-opacity', 0)
            )
            .text(d => d.label);

        (_transition ? _selection.transition(_transition) : _selection)
            .call(apply_attributes)
            .call(apply_styles);

        return _selection;

    }

    const _label: EdgeLabelFunction = Object.assign(_function, {
        attr,
        style,
        selector,
        transition,
        exit: exit_label
    });

    return _label;

    function attr (a, v?) {
        if (arguments.length === 1) return _attributes.get(a);
        _attributes.set(a, v);
        return _label;
    }

    function style (s, v?) {
        if (arguments.length === 1) return _styles.get(s);
        _styles.set(s, v);
        return _label;
    }

    function selector (s?) {
        if (!arguments.length) return _selector;
        _selector = s;
        return _label;
    }

    function transition (transition?) {
        if (!arguments.length) return _transition;
        _transition = transition;
        return _label;
    }

    function apply_attributes (selection) {
        _attributes.forEach((value, attr) => selection.attr(attr, value));
    }

    function apply_styles (selection) {
        _styles.forEach((value, style) => selection.style(style, value));
    }

    function exit_label (selection) {
        (_transition ? selection.transition(_transition) : selection)
            .style('fill-opacity', 0)
            .style('stroke-opacity', 0);
    }


}
