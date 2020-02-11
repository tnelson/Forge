import * as d3 from 'd3';
function edge() {
    let _selection = null;
    let _scheme = null;
    let _transition = d3.transition().duration(0);
    // Arrow properties
    let _arrow_width = 3, _arrow_height = 10, _arrow_offset = 2;
    // Edge properties
    let _line = d3.line()
        .x(d => d.x)
        .y(d => d.y)
        .curve(d3.curveBasis);
    // Font properties
    let _fontsize = 16;
    function _function(selection) {
        // Add new groups
        let enter = selection
            .enter()
            .append('g')
            .attr('class', 'edge');
        enter
            .attr('opacity', 0)
            .transition(_transition)
            .attr('opacity', 1);
        // Add all elements to enter selection
        enter
            .call(_enter_paths)
            .call(_enter_arrows)
            .call(_enter_rects)
            .call(_enter_labels);
        // Update existing elements
        selection
            .call(_update_paths)
            .call(_update_arrows)
            .call(_update_rects)
            .call(_update_labels);
        // Remove exiting groups
        selection
            .exit()
            .transition(_transition)
            .attr('opacity', 0)
            .remove();
        _selection = enter.merge(selection);
        return _selection;
    }
    const _edge = Object.assign(_function, {
        fontSize,
        highlight,
        points,
        scheme,
        transition
    });
    return _edge;
    function fontSize(size) {
        if (!arguments.length)
            return _fontsize;
        _fontsize = size;
        return _edge;
    }
    function highlight(edge) {
        // Bring the supplied edge to the top
        let e = d3.select(edge);
        e.raise();
        // If there's no selection for some reason, just return
        if (!_selection)
            return;
        if (edge === null) {
            // Return all edges back to normal
            _selection
                .each(_make_normal);
        }
        else {
            // Dim all others while we highlight the provided edge
            if (_selection) {
                // Get the highlight group
                let datum = e.datum();
                let group = _scheme_group(datum);
                if (!group) {
                    _selection
                        .each(function () {
                        this === edge
                            ? _make_highlighted.call(this)
                            : _make_dimmed.call(this);
                    });
                }
                else {
                    _selection.each(function () {
                        if (this === edge) {
                            _make_highlighted.call(this);
                        }
                        else if (_scheme_group(d3.select(this).datum()) === group) {
                            _make_lowlighted.call(this);
                        }
                        else {
                            _make_dimmed.call(this);
                        }
                    });
                }
            }
        }
    }
    function points() {
        if (!_selection)
            return [];
        let points = [];
        _selection.each(function (d) {
            d.points.forEach(point => {
                points.push({
                    x: point.x,
                    y: point.y,
                    element: this
                });
            });
        });
        return points;
    }
    function scheme(scheme) {
        if (!arguments.length)
            return _scheme;
        _scheme = scheme;
        return _edge;
    }
    function transition(transition) {
        if (!arguments.length)
            return _transition;
        _transition = transition;
        return _edge;
    }
    function _enter_arrows(enter) {
        enter
            .append('path')
            .attr('class', 'arrow')
            .attr('d', arrow_head(_arrow_width, _arrow_height, _arrow_offset))
            .attr('transform', arrow_transform)
            .attr('stroke', _stroke_color)
            .attr('fill', _stroke_color);
    }
    function _enter_labels(enter) {
        enter
            .append('text')
            .attr('class', 'label')
            .attr('x', d => d.x)
            .attr('y', d => d.y)
            .attr('text-anchor', 'middle')
            .attr('dy', '0.31em')
            .attr('fill', _stroke_color)
            .attr('font-size', `${_fontsize}px`)
            .text(d => d.label);
    }
    function _enter_paths(enter) {
        enter
            .append('path')
            .attr('class', 'edge')
            .attr('d', d => _line(d.points))
            .attr('fill', 'none')
            .attr('stroke', _stroke_color)
            .attr('stroke-width', 2)
            .transition(_transition)
            .attrTween('stroke-dasharray', function () {
            let l = this.getTotalLength(), i = d3.interpolateString(`0,${l}`, `${l},${l}`);
            return t => i(t);
        })
            .on('end', function () {
            d3.select(this)
                .attr('stroke-dasharray', null);
        })
            .on('interrupt', function () {
            d3.select(this)
                .attr('stroke-dasharray', null);
        });
    }
    function _enter_rects(enter) {
        enter
            .append('rect')
            .attr('class', 'bg')
            .attr('display', 'none');
    }
    function _update_arrows(update) {
        update
            .select('path.arrow')
            .transition(_transition)
            .attr('stroke', _stroke_color)
            .attr('fill', _stroke_color)
            .attr('transform', arrow_transform);
    }
    function _update_labels(update) {
        update
            .select('text.label')
            .transition(_transition)
            .attr('x', d => d.x)
            .attr('y', d => d.y)
            .attr('fill', _stroke_color)
            .attr('font-size', `${_fontsize}px`)
            .text(d => d.label);
    }
    function _update_paths(update) {
        update
            .select('path.edge')
            .transition(_transition)
            .attr('stroke', _stroke_color)
            .attr('d', d => _line(d.points));
    }
    function _update_rects(update) {
        update
            .select('rect.bg')
            .transition(_transition)
            .attr('x', d => d.x)
            .attr('y', d => d.y);
    }
    function _make_highlighted() {
        let edge = d3.select(this);
        let text = edge.select('text')
            .attr('display', null);
        edge.select('path.edge')
            .attr('stroke', _stroke_color)
            .attr('stroke-width', 4);
        edge.select('path.arrow')
            .attr('d', arrow_head(5, _arrow_height, _arrow_offset))
            .attr('stroke', _stroke_color)
            .attr('fill', _stroke_color);
        let bbox = text.node().getBBox();
        edge.select('rect.bg')
            .attr('x', (d) => d.x - bbox.width / 2)
            .attr('y', (d) => d.y - bbox.height / 2)
            .attr('width', bbox.width)
            .attr('height', bbox.height)
            .attr('stroke', 'none')
            .attr('fill', 'white')
            .attr('fill-opacity', 0.8)
            .attr('display', null);
    }
    function _make_lowlighted() {
        let edge = d3.select(this);
        edge.select('text')
            .attr('display', 'none');
        edge.select('path.edge')
            .attr('stroke', _stroke_color)
            .attr('stroke-width', 2);
        edge.select('path.arrow')
            .attr('d', arrow_head(_arrow_width, _arrow_height, _arrow_offset))
            .attr('stroke', _stroke_color)
            .attr('fill', _stroke_color);
        edge.select('rect.bg')
            .attr('display', 'none');
    }
    function _make_dimmed() {
        let edge = d3.select(this);
        edge.select('text')
            .attr('display', 'none');
        edge.select('path.edge')
            .attr('stroke', '#ccc')
            .attr('stroke-width', 2);
        edge.select('path.arrow')
            .attr('d', arrow_head(_arrow_width, _arrow_height, _arrow_offset))
            .attr('stroke', '#ccc')
            .attr('fill', '#ccc');
        edge.select('rect.bg')
            .attr('display', 'none');
    }
    function _make_normal() {
        let edge = d3.select(this);
        edge.select('text')
            .attr('display', null)
            .style('font-size', null);
        edge.select('path.edge')
            .attr('stroke', _stroke_color)
            .attr('stroke-width', 2);
        edge.select('path.arrow')
            .attr('d', arrow_head(_arrow_width, _arrow_height, _arrow_offset))
            .attr('stroke', _stroke_color)
            .attr('fill', _stroke_color);
        edge.select('rect.bg')
            .attr('display', 'none');
    }
    function _stroke_color(d) {
        if (!_scheme)
            return d3.color('black');
        return _scheme.colors[d.data.id()] || d3.color('black');
    }
    function _scheme_group(d) {
        if (!_scheme || !d)
            return null;
        return _scheme.groups[d.data.id()] || null;
    }
}
function arrow_head(w, h, o) {
    return `M -${h - o} -${w} L ${o} 0 L -${h - o} ${w} z`;
}
function arrow_transform(d) {
    let points = d.points;
    if (points.length > 1) {
        let p2 = points[points.length - 2];
        let p1 = points[points.length - 1];
        let angle = Math.atan2(p1.y - p2.y, p1.x - p2.x) * (180 / Math.PI);
        return `translate(${p1.x},${p1.y}) rotate(${angle})`;
    }
    return null;
}
export { edge };
