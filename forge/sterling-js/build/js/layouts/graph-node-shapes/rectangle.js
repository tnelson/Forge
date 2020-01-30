export function rectangle() {
    let _selection, _attributes = new Map(), _styles = new Map(), _transition = null;
    _attributes
        .set('height', d => d.height ? d.height : 50)
        .set('shape-rendering', 'geometricPrecision')
        .set('width', d => d.width ? d.width : 150)
        .set('x', d => d.width ? -d.width / 2 : 75)
        .set('y', d => d.height ? -d.height / 2 : 25);
    _styles
        .set('stroke', '#000')
        .set('fill', '#fff');
    function _function(selection) {
        _selection = selection
            .selectAll('rect')
            .data(d => [d])
            .join(enter => enter.append('rect')
            .call(apply_attributes)
            .call(apply_styles)
            .attr('x', 0)
            .attr('y', 0)
            .attr('width', 0)
            .attr('height', 0), update => update, exit => exit
            .call(exit_rects)
            .remove());
        (_transition ? _selection.transition(_transition) : _selection)
            .call(apply_attributes)
            .call(apply_styles);
        return _selection;
    }
    const _rectangle = Object.assign(_function, {
        attr,
        style,
        transition,
        exit: exit_rects
    });
    return _rectangle;
    function attr(a, v) {
        if (arguments.length === 1)
            return _attributes.get(a);
        _attributes.set(a, v);
        return _rectangle;
    }
    function style(s, v) {
        if (arguments.length === 1)
            return _styles.get(s);
        _styles.set(s, v);
        return _rectangle;
    }
    function transition(transition) {
        if (!arguments.length)
            return _transition;
        _transition = transition;
        return _rectangle;
    }
    function apply_attributes(selection) {
        _attributes.forEach((value, attr) => selection.attr(attr, value));
    }
    function apply_styles(selection) {
        _styles.forEach((value, style) => selection.style(style, value));
    }
    function exit_rects(selection) {
        (_transition ? selection.transition(_transition) : selection)
            .attr('x', 0)
            .attr('y', 0)
            .attr('width', 0)
            .attr('height', 0);
    }
}
