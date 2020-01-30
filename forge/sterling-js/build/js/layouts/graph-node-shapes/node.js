import * as d3 from 'd3';
function node() {
    let _selection = null;
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
        return _selection;
    }
    const _node = Object.assign(_function, {
        transition
    });
    return _node;
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
            .attr('fill', 'white')
            .attr('font-size', '16px')
            .attr('font-weight', 'bold')
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
            .attr('stroke', '#455a64')
            .attr('stroke-width', 2)
            .attr('fill', '#708690');
    }
    function _update_labels(update) {
    }
}
export { node };
