import * as d3 from 'd3';
export function line() {
    let _l = d3.line()
        .x(d => d.x)
        .y(d => d.y)
        .curve(d3.curveBasis);
    let _lines, _stroke = '#000', _stroke_width = 1;
    function _line(selection) {
        _lines = selection
            .selectAll('path')
            .data(d => [d.points]);
        _lines
            .exit()
            .remove();
        _lines = _lines
            .enter()
            .append('path')
            .merge(_lines);
        _lines
            .attr('d', _l);
        _lines
            .style('fill', 'none')
            .style('stroke', _stroke)
            .style('stroke-width', _stroke_width);
        return _lines;
    }
    return _line;
}
