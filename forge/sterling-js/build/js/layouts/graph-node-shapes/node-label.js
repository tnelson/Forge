export function node_label() {
    let _selection, _attributes = new Map(), _styles = new Map(), _transition = null;
    _attributes
        .set('dx', _dx)
        .set('dy', _dy)
        .set('text-anchor', _anchor)
        .set('text-rendering', 'geometricPrecision')
        .set('x', _x)
        .set('y', _y);
    _styles
        .set('fill', 'black')
        .set('fill-opacity', 1)
        .set('font-size', '12px')
        .set('font-weight', 'regular')
        .set('stroke', 'none')
        .set('stroke-opacity', 1);
    let _placement = 'c';
    function _function(selection) {
        _selection = selection
            .selectAll('text')
            .data(d => [d])
            .join(enter => enter.append('text')
            .call(apply_attributes)
            .call(apply_styles)
            .attr('x', 0)
            .attr('y', 0)
            .style('fill-opacity', 0)
            .style('stroke-opacity', 0))
            .text(d => d.data);
        (_transition ? _selection.transition(_transition) : _selection)
            .call(apply_attributes)
            .call(apply_styles);
        return _selection;
    }
    const _label = Object.assign(_function, {
        attr,
        style,
        placement,
        transition,
        exit: exit_label
    });
    return _label;
    function attr(a, v) {
        if (arguments.length === 1)
            return _attributes.get(a);
        _attributes.set(a, v);
        return _label;
    }
    function style(s, v) {
        if (arguments.length === 1)
            return _styles.get(s);
        _styles.set(s, v);
        return _label;
    }
    function placement(placement) {
        if (!arguments.length)
            return _placement;
        _placement = placement;
        return _label;
    }
    function transition(transition) {
        if (!arguments.length)
            return _transition;
        _transition = transition;
        return _label;
    }
    function apply_attributes(selection) {
        _attributes.forEach((value, attr) => selection.attr(attr, value));
    }
    function apply_styles(selection) {
        _styles.forEach((value, style) => selection.style(style, value));
    }
    function exit_label(selection) {
        (_transition ? selection.transition(_transition) : selection)
            .attr('x', 0)
            .attr('y', 0)
            .style('fill-opacity', 0)
            .style('stroke-opacity', 0);
    }
    function _x(d) {
        let width = d.width ? d.width : 0;
        switch (_placement) {
            case 'c':
                return 0;
            case 'bl':
            case 'tl':
                return -width / 2;
            case 'br':
            case 'tr':
                return width / 2;
            default:
                return 0;
        }
    }
    function _y(d) {
        let height = d.height ? d.height : 0;
        switch (_placement) {
            case 'c':
                return 0;
            case 'bl':
            case 'br':
                return height / 2;
            case 'tl':
            case 'tr':
                return -height / 2;
            default:
                return 0;
        }
    }
    function _dx() {
        switch (_placement) {
            case 'c':
                return 0;
            case 'bl':
            case 'tl':
                return '1em';
            case 'br':
            case 'tr':
                return '-1em';
            default:
                return 0;
        }
    }
    function _dy() {
        switch (_placement) {
            case 'c':
                return '0.31em';
            case 'bl':
            case 'br':
                return '-1em';
            case 'tl':
            case 'tr':
                return '1.62em';
            default:
                return '0.31em';
        }
    }
    function _anchor() {
        switch (_placement) {
            case 'c':
                return 'middle';
            case 'bl':
            case 'tl':
                return 'start';
            case 'br':
            case 'tr':
                return 'end';
            default:
                return 'middle';
        }
    }
}
