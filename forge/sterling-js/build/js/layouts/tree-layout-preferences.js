export class TreeLayoutPreferences {
    constructor() {
        this.font_size = 14;
        this.font_weight = 'normal';
        this.font_family = 'monospace';
        this.text_dy = '0.31em';
        this.text_anchor = 'start';
        this.text_lower_stroke_linejoin = 'round';
        this.text_lower_stroke_width = 3;
        this.link_stroke = '#555';
        this.link_stroke_opacity = 0.4;
        this.link_stroke_width = 1.5;
        this.margin = {
            top: 0,
            right: 250,
            bottom: 0,
            left: 150
        };
        this.node_radius = 4;
        this.node_fill = '#555';
        this.node_stroke_width = 10;
        this.node_text_separation = 8;
        this.show_builtins = false;
        this.transition_duration = 350;
    }
    font_attributes() {
        return {
            'font-family': this.font_family,
            'font-size': this.font_size,
            'font-weight': this.font_weight
        };
    }
    link_stroke_attributes() {
        return {
            'stroke': this.link_stroke,
            'stroke-opacity': this.link_stroke_opacity,
            'stroke-width': this.link_stroke_width
        };
    }
    node_attributes() {
        return {
            'r': this.node_radius,
            'fill': this.node_fill,
            'stroke-width': this.node_stroke_width
        };
    }
    text_attributes() {
        return {
            'dy': this.text_dy,
            'text-anchor': this.text_anchor
        };
    }
    text_lower_attributes() {
        return Object.assign(Object.assign({}, this.text_attributes()), { 'stroke-linejoin': this.text_lower_stroke_linejoin, 'stroke-width': this.text_lower_stroke_width });
    }
}
