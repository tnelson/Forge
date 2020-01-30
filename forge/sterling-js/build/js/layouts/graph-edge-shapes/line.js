import * as d3 from 'd3';
export function line() {
    let _selection, _attributes = new Map(), _styles = new Map(), _transition = null;
    let _l = d3.line()
        .x(d => d.x)
        .y(d => d.y)
        .curve(d3.curveBasis);
    _styles
        .set('fill', 'none')
        .set('stroke', '#000')
        .set('stroke-width', 1);
    function _function(selection) {
        _selection = selection
            .selectAll('path')
            .data(d => [d.points])
            .join(enter => enter.append('path')
            .attr('d', _l)
            .call(enter => _transition ? transition_enter(enter) : enter), update => update
            .call(update => _transition ? transition_update(update) : update));
        _transition = null;
        _selection
            .call(apply_attributes)
            .call(apply_styles);
        return _selection;
    }
    const _line = Object.assign(_function, {
        attr,
        style,
        transition,
        exit: transition_exit
    });
    return _line;
    function attr(a, v) {
        if (arguments.length === 1)
            return _attributes.get(a);
        _attributes.set(a, v);
        return _line;
    }
    function style(s, v) {
        if (arguments.length === 1)
            return _styles.get(s);
        _styles.set(s, v);
        return _line;
    }
    function transition(transition) {
        if (!arguments.length)
            return _transition;
        _transition = transition;
        return _line;
    }
    function transition_exit(exit) {
        if (_transition) {
            exit
                .attr('stroke-opacity', 1)
                .transition(_transition)
                .attr('stroke-opacity', 0);
        }
    }
    function transition_enter(enter) {
        return enter
            .attr('stroke-opacity', 0)
            .transition(_transition)
            .attr('stroke-opacity', 1)
            .attrTween('stroke-dasharray', tween_stroke_in)
            .on('end', end_tween_stroke)
            .on('interrupt', end_tween_stroke);
    }
    function transition_update(update) {
        return update
            .transition(_transition)
            .attr('d', _l);
    }
    function tween_stroke_in() {
        let l = this.getTotalLength(), i = d3.interpolateString(`0,${l}`, `${l},${l}`);
        return t => i(t);
    }
    function end_tween_stroke() {
        d3.select(this).attr('stroke-dasharray', null);
    }
    function apply_attributes(selection) {
        _attributes.forEach((value, attr) => selection.attr(attr, value));
    }
    function apply_styles(selection) {
        _styles.forEach((value, style) => selection.style(style, value));
    }
}
