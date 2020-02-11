import * as d3 from 'd3';
function node() {
    let _fontsize = 16;
    let _selection = null;
    let _scheme = null;
    let _transition = d3.transition().duration(0);
    function _function(selection) {
        // Add new groups
        let enter = selection
            .enter()
            .append('g')
            .attr('class', 'node')
            .attr('transform', d => `translate(${d.x},${d.y})`);
        enter
            .attr('opacity', 0)
            .transition(_transition)
            .attr('opacity', 1);
        // Add all elements to enter selection
        enter
            .call(_enter_rects)
            .call(_enter_labels);
        // Update existing elements
        selection
            .call(_update_rects)
            .call(_update_labels);
        // Remove exiting groups
        selection
            .exit()
            .transition(_transition)
            .attr('opacity', 0)
            .remove();
        _selection = enter
            .merge(selection)
            .transition(_transition)
            .attr('transform', d => `translate(${d.x},${d.y})`);
        // _selection.on('interrupt', () => _selection.attr('opacity', 1));
        return _selection;
    }
    const _node = Object.assign(_function, {
        fontSize,
        scheme,
        transition
    });
    return _node;
    function fontSize(size) {
        if (!arguments.length)
            return _fontsize;
        _fontsize = size;
        return _node;
    }
    function scheme(scheme) {
        if (!arguments.length)
            return _scheme;
        _scheme = scheme;
        return _node;
    }
    function transition(transition) {
        if (!arguments.length)
            return _transition;
        _transition = transition;
        return _node;
    }
    function _enter_labels(enter) {
        enter
            .append('text')
            .attr('class', 'label')
            .attr('text-anchor', 'middle')
            .attr('dy', '0.31em')
            .attr('stroke', 'none')
            .attr('fill', d => d.color ? text_color(d.color) : 'black')
            .attr('font-weight', 'bold')
            .attr('font-size', `${_fontsize}px`)
            .text(d => d.data);
    }
    function _enter_rects(enter) {
        enter
            .append('rect')
            .attr('class', 'shape')
            .attr('x', d => -d.width / 2)
            .attr('y', d => -d.height / 2)
            .attr('rx', 2)
            .attr('width', d => d.width)
            .attr('height', d => d.height)
            .attr('stroke', d => _bg_color(d).darker())
            .attr('stroke-width', 2)
            .attr('fill', _bg_color);
    }
    function _update_labels(update) {
        update.attr('font-size', `${_fontsize}px`);
    }
    function _update_rects(update) {
        update
            .select('rect')
            .attr('x', d => -d.width / 2)
            .attr('y', d => -d.height / 2)
            .attr('width', d => d.width)
            .attr('height', d => d.height)
            .attr('stroke', d => _bg_color(d).darker())
            .attr('fill', _bg_color);
    }
    function _bg_color(d) {
        if (!_scheme)
            return d3.color('white');
        return _scheme.colors[d.data.id()] || d3.color('white');
    }
}
function text_color(background_color) {
    return d3.hsl(d3.color(background_color)).l > 0.5 ? '#000' : '#fff';
}
export { node };
