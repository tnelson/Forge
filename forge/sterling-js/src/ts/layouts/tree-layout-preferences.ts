export class TreeLayoutPreferences {

    font_size: number = 14;
    font_weight: string = 'normal';
    font_family: string = 'monospace';

    text_dy: string = '0.31em';
    text_anchor: string = 'start';
    text_lower_stroke_linejoin: string = 'round';
    text_lower_stroke_width: number = 3;

    link_stroke: string = '#555';
    link_stroke_opacity: number = 0.4;
    link_stroke_width: number = 1.5;

    margin: {
        top, right, bottom, left: number
    } = {
        top: 0,
        right: 250,
        bottom: 0,
        left: 150
    };

    node_radius: number = 4;
    node_fill: string = '#555';
    node_stroke_width: number = 10;
    node_text_separation: number = 8;

    show_builtins: boolean = false;
    transition_duration: number = 350;

    font_attributes () {
        return {
            'font-family': this.font_family,
            'font-size': this.font_size,
            'font-weight': this.font_weight
        };
    }

    link_stroke_attributes () {
        return {
            'stroke': this.link_stroke,
            'stroke-opacity': this.link_stroke_opacity,
            'stroke-width': this.link_stroke_width
        }
    }

    node_attributes () {
        return {
            'r': this.node_radius,
            'fill': this.node_fill,
            'stroke-width': this.node_stroke_width
        }
    }

    text_attributes () {
        return {
            'dy': this.text_dy,
            'text-anchor': this.text_anchor
        };
    }

    text_lower_attributes () {
        return {
            ...this.text_attributes(),
            'stroke-linejoin': this.text_lower_stroke_linejoin,
            'stroke-width': this.text_lower_stroke_width
        }
    }

}
