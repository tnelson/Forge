export function arrow() {
    let _selection, _attributes = new Map(), _styles = new Map(), _transition = null;
    let _w = 3, _h = 10, _o = 2;
    _styles
        .set('stroke', 'black')
        .set('fill', 'black');
    function _function(selection) {
        _selection = selection
            .selectAll('path.arrow')
            .data(d => [d])
            .join(enter => enter.append('path')
            .attr('class', 'arrow')
            .attr('d', `M -${_h - _o} -${_w} L ${_o} 0 L -${_h - _o} ${_w} z`)
            .attr('transform', _transform)
            .call(enter => _transition ? transition_enter(enter) : enter));
        _selection
            .call(apply_attributes)
            .call(apply_styles);
    }
    const _arrow = Object.assign(_function, {
        attr,
        height,
        style,
        width,
        transition,
        exit: transition_exit
    });
    return _arrow;
    function _transform(d) {
        let points = d.points;
        if (points.length > 1) {
            let prev = points[points.length - 2];
            let edge = points[points.length - 1];
            let angle = find_angle(edge, prev);
            return `translate(${edge.x},${edge.y}) rotate(${angle})`;
        }
        return null;
    }
    function attr(a, v) {
        if (arguments.length === 1)
            return _attributes.get(a);
        _attributes.set(a, v);
        return _arrow;
    }
    function height(height) {
        if (!arguments.length)
            return _h;
        _h = +height;
        return _arrow;
    }
    function style(s, v) {
        if (arguments.length === 1)
            return _styles.get(s);
        _styles.set(s, v);
        return _arrow;
    }
    function transition(transition) {
        if (!arguments.length)
            return _transition;
        _transition = transition;
        return _arrow;
    }
    function transition_enter(enter) {
        return enter
            .style('stroke-opacity', 0)
            .style('fill-opacity', 0)
            .transition(_transition)
            .style('stroke-opacity', 1)
            .style('fill-opacity', 1);
    }
    function transition_exit(exit) {
        if (_transition) {
            exit
                .style('stroke-opacity', 1)
                .style('fill-opacity', 1)
                .transition(_transition)
                .style('stroke-opacity', 0)
                .style('fill-opacity', 0);
        }
    }
    function width(width) {
        if (!arguments.length)
            return _w;
        _w = +width;
        return _arrow;
    }
    function apply_attributes(selection) {
        _attributes.forEach((value, attr) => selection.attr(attr, value));
    }
    function apply_styles(selection) {
        _styles.forEach((value, style) => selection.style(style, value));
    }
}
function find_angle(p1, p2) {
    return Math.atan2(p1.y - p2.y, p1.x - p2.x) * (180 / Math.PI);
}
