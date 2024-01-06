const d3 = require('d3')
// At the moment, if using base d3, the require needs to be the first line. 

// constants for our visualization
const BASE_X = 150;
const BASE_Y = 100;
const TIMESLOT_HEIGHT = 60;
const AGENT_WIDTH = 270;
const BOX_HEIGHT = 130;
const BOX_WIDTH = 200;
const LINE_HEIGHT = 20;

// colors
const RED = '#E54B4B';
const BLUE = '#0495C2';
const GREEN = '#19EB0E';
const BLACK = '#000000';

// allows for custom fonts
d3.select(svg)
    .append('defs')
    .append('style')
    .attr('type', 'text/css')
    .text("@import url('https://fonts.googleapis.com/css?family=Open+Sans:400,300,600,700,800');");

/**
 * A function to grab the timeslot data from the forge spec and 
 * store these timeslots in order.
 * @param {*} arr the array to populate with the ordered timeslots
 */
function orderTimeslots(arr) {
    // grabbing the data from the forge spec
    const nextRange = Timeslot.next.tuples().map(x => x.toString());
    const first = Timeslot.atoms(true).filter(timeslot => !nextRange.includes(timeslot.toString()))[0];
    // putting the timeslots in order
    let i;
    let curr = first;
    for (i = 0; i < Timeslot.atoms(true).length; i++) {
        arr.push(curr);
        curr = curr.next;   
    }
}

// data from forge spec
const strands = strand.atoms(true);
const messages = Timeslot.atoms(true);
const agentNames = strands.map(x => x.toString());
const keyNames = Key.atoms(true).map(x => x.toString());
const timeslots = [];

// populating the timeslots array
orderTimeslots(timeslots);

// map from role -> name
const roles = {}
// map from Timeslot -> Agent -> [Data]
const learnedInformation = {};
// map from Timeslot -> Agent -> [Data]
const generatedInformation = {};
// map from Timeslot -> Agent -> Boolean
const visibleInformation = {};
// map from Datum -> Message
const dataMessageMap = {};
// map from public key (Datum) -> owner (Agent[])
const pubKeyMap = {};
// map from private key (Datum) -> owner (Agent[])
const privKeyMap = {};
// map from Datum -> owners
const ltksMap = {};
const ciphertextMap = {};
const cipherKeyMap = {};

// populating the roles object
agent.tuples().forEach(x => {
    let role = x.atoms()[0].toString();
    let name = x.atoms()[1].toString();
    roles[role] = name;
});

// populating the learnedInformation and generatedInformation objects
strands.forEach((strand) => {

    let s = strand.toString();

    // grab the learned_times data from the forge spec
    const learned = strand.agent.learned_times.tuples().map(tuple => tuple.atoms());

    learned.map((info) => {
        // unpack the information
        let d = info[0].toString();
        let ts = info[1].toString();

        if (!learnedInformation[ts]) {
            learnedInformation[ts] = {};
        }

        if (!learnedInformation[ts][s]) {
            learnedInformation[ts][s] = [];
        }

        if (ts !== "Timeslot0" || (!agentNames.includes(d) && !keyNames.includes(d))) {
            // store the information in our learnedInformation object
            learnedInformation[ts][s].push(d);
        }
    });

    // grab the generated_times data from the forge spec
    const generated = strand.agent.generated_times.tuples().map(tuple => tuple.atoms());

    generated.map((info) => {
        // unpack the information
        let d = info[0].toString();
        let ts = info[1].toString();

        if (!generatedInformation[ts]) {
            generatedInformation[ts] = {};
        }

        if (!generatedInformation[ts][s]) {
            generatedInformation[ts][s] = [];
        }

        // store the information in our generatedInformation object
        generatedInformation[ts][s].push(d);

    })
});

// populating the visibleInformation object (initializing everything with false)
timeslots.forEach((timeslot) => {
    const ts = timeslot.toString();
    strands.forEach((strand) => {
        const s = strand.toString();

        if (!visibleInformation[ts]) {
            visibleInformation[ts] = {};
        }

        visibleInformation[ts][s] = false;
    });
});

// populating the dataMessageMap object
data.tuples().forEach((tuple) => {
    let m = tuple.atoms()[0];
    let d = tuple.atoms()[1].toString();
    dataMessageMap[d] = m;
});

// populating privKeyMap
KeyPairs0.owners.tuples().forEach(x => {
    let atoms = x.atoms();
    let key = atoms[0].toString();
    let owner = atoms[1].toString();
    if (!privKeyMap[key]) {
        privKeyMap[key] = [];  
    } 
    privKeyMap[key].push(owner);
});

// populating pubKeyMap
KeyPairs0.pairs.tuples().forEach(x => {
    let atoms = x.atoms();
    let private = atoms[0].toString();
    let public = atoms[1].toString();
    let owners = privKeyMap[private]; 
    if (!pubKeyMap[public]) {
        pubKeyMap[public] = [];  
    } 
    pubKeyMap[public].push(owners);
});

// populating the ltksMap object
KeyPairs0.ltks.tuples().forEach(x => {
    let s = x.toString();
    let arr = s.split(", ");
    let key = arr[2];
    let val = arr[0] + " " + arr[1];
    if (!ltksMap[key]) {
        ltksMap[key] = [];  
    } 
    ltksMap[key].push(val);
});

// populating the ciphertextMap object
plaintext.tuples().forEach((tuple) => {
    let atoms = tuple.atoms();
    let key = atoms[0].toString();
    let val = atoms[1];

    if (!ciphertextMap[key]) {
        ciphertextMap[key] = [];
    }

    ciphertextMap[key].push(val);  
});

// populating the cipherKeyMap object
encryptionKey.tuples().forEach((tuple) => {
    let atoms = tuple.atoms();
    let key = atoms[0].toString();
    let val = atoms[1].toString();

    // TODO: just do the lookup here!!!!!!! :^)
    cipherKeyMap[key] = val;
});


/**
 * gets the names of the timeslots before the given one
 * @param {*} timeslot - a Timeslot prop from the forge spec
 * @returns an array of strings
 */
function getTimeSlotsBefore(timeslot) {
    const sliceIndex = timeslots.indexOf(timeslot);
    return timeslots.slice(0, sliceIndex).map(t => t.toString());
}

/**
 * a function to get the x coordinate of a given agent
 * @param {Object} agent - an agent prop from the forge spec
 */
function x(agent) {
    return BASE_X + (agentNames.indexOf(agent.toString()) * AGENT_WIDTH);
}

/**
 * a function to get the y coordinate of a given timeslot
 * @param {Object} timeslot - a timeslot prop from the forge spec
 */
function y(timeslot) {

    let visibleNum = 0;
    const previousTimeslots = getTimeSlotsBefore(timeslot);

    previousTimeslots.forEach((ts) => {
        // if there is an agent with info visible in this timeslot, count it
        let i;
        let v = false;
        for (i = 0; i < strands.length; i++) {
            let a = strands[i].toString();
            if (visibleInformation[ts][a]) {
                v = true;
            }
        }
        if (v) {
            visibleNum++;
        }
    });
 
    return BASE_Y + (timeslots.indexOf(timeslot) * TIMESLOT_HEIGHT) + (visibleNum * BOX_HEIGHT);
}

/**
 * a function to get the starting x coordinate for a message line. 
 * This corresponds with the x coordinate of the message SENDER.
 * @param {Object} m - a message prop from the forge spec
 */
function messageX1(m) {
    return x(m.sender);
}

/**
 * a function to get the ending x coordinate for a message line.
 * This corresponds with the x coordinate of the message RECEIVER.
 * @param {Object} m - a message prop from the forge spec
 */
function messageX2(m) {
    return x(m.receiver);
}

/**
 * a function to get the starting y coordinate for a message line.
 * This corresponds with the y coordinate of the SEND time.
 * @param {Object} m - a message prop from the forge spec
 */
function messageY1(m) {
    return y(m);
}

/**
 * a function to get the ending y coordinate for a message line.
 * This corresponds with the y coordinate of the RECEIVE time.
 * @param {Object} m - a message prop from the forge spec
 */
function messageY2(m) {
    return y(m); // Y1 and Y2 are sendTime because of model change 
}

/**
 * a function to compute the x value of a label based on its parent's position
 * @returns the computed x value
 */
function labelX() {
    const l = d3.select(this.parentNode).select('line');
    // grabbing the values of the x1, x2, and y1 attributes
    const labelX1 = parseInt(l.attr('x1'));
    const labelX2 = parseInt(l.attr('x2'));
    // calculating and returing the x value for the message's label
    return (labelX1 + labelX2) / 2.0;
}

/**
 * a function to compute the y value of a label based on its parent's position
 * @returns the computed y value
 */
function labelY() {
    const l = d3.select(this.parentNode).select('line');
    return parseInt(l.attr('y1')) - 20;
}

function parsedTermsToString(parsedTerms, key) {

    let s = (key === "") ? "" : "{ ";

    let i;
    for (i = 0; i < parsedTerms.length; i++) {

        let term = parsedTerms[i];

        if (term["subscript"]) {
            s += parsedTermsToString(term.content, term.subscript);
        } else {
            s += term.content; 
        }

        s += " "
    }

    if (key !== "") {
        s += `}[${key}]`
    }

    return s;
}

function parseKey(itemString, prefix, map) {
    const owners = map[itemString];
    let s;
    
    if (owners.length === 1) {
        return {
            content: `${prefix}(${owners[0]})`
        };
    } else {
        return {
            content: `${prefix}(SHARED)`,
            shared: owners
        };
        
    }
}

function parseTerms(items) {

    const newItems = items.map((item) => {

        const itemString = item.toString();

        if (pubKeyMap[itemString]) {
            return parseKey(itemString, "pubK", pubKeyMap);
        } else if (privKeyMap[itemString]) {
            return parseKey(itemString, "privK", privKeyMap);
        } else if (ltksMap[itemString]) {
            return parseKey(itemString, "ltk", ltksMap);
        } else if (itemString.includes("Ciphertext")) {
            const pt = ciphertextMap[itemString];
            const key = cipherKeyMap[itemString]; // in progress see TODO above
            const parsedKey = parseTerms([key])[0];

            return {
                content: parseTerms(pt),
                subscript: parsedKey
            }
        } else {
            return {
                content: itemString
            };
        }
    });

    return newItems;
}

function flattenParsedTerms(parsedTerms) {
    let array = [];
    let i;
    for (i = 0; i < parsedTerms.length; i++) {
        let term = parsedTerms[i];

        if (term["subscript"]) {
            array.push({content: "{"});
            array = array.concat(flattenParsedTerms(term.content));
            array.push({content: "}"});
            
            if (term.subscript["shared"]) {
                array.push({
                    content: term.subscript.content,
                    shared: term.subscript.shared,
                    subscript: "subscript"
                });
            } else {
                array.push({
                    content: term.subscript.content,
                    subscript: "subscript"
                });
            }

        } else if (term["shared"]){
            array.push({
                content: term.content,
                shared: term.shared
            });
        } else {
            array.push({content: term.content});
        } 
    }

    return array;
}

function onSharedMouseEnter(x, y, owners) {
    d3.select(svg).append('text')
        .attr("x", x)
        .attr("y", y - 15)
        .attr("class", "sharedLabel")
        .text(owners);  
}

function onSharedMouseLeave() {
    d3.selectAll(".sharedLabel").remove();
    // just delete all w the appropriate calss  
}

// should probably return a width
function printFlattenedTerms(terms, container, x, y, color, shouldPrint) {

    let i;
    let w = 0;
    for (i = 0; i < terms.length; i++) {

        let term = terms[i];
        let newText;

        if (term["subscript"] && term["shared"]) {
            newText = container.append('text')
                .style('font-size', 12)
                .attr('x', x + w)
                .attr('y', y)
                .attr('dx', 5)
                .attr('dy', 5)
                .style('fill', color)
                .text(term.content)
                .on('mouseenter', () => onSharedMouseEnter(x, y, term.shared))
                .on('mouseleave', onSharedMouseLeave)
                .style('cursor', 'pointer');
            
            w += newText.node().getComputedTextLength() + 5;
        } else if (term["subscript"]) {
            newText = container.append('text')
                .text(term.content)
                .style('font-size', 12)
                .attr('x', x + w)
                .attr('y', y)
                .attr('dx', 5)
                .attr('dy', 5)
                .style('fill', color);
            
            w += newText.node().getComputedTextLength() + 5;

        } else if (term["shared"]) {
            // TODO: hover
            newText = container.append('text')
                .attr('x', x + w)
                .attr('y', y)
                .style('font-family', '"Open Sans", sans-serif')
                .style('fill', color)
                .text(term.content)
                .on('mouseenter', () => onSharedMouseEnter(x, y, term.shared))
                .on('mouseleave', onSharedMouseLeave)
                .style('cursor', 'pointer');
            
            w += newText.node().getComputedTextLength();
        } else {
            newText = container.append('text')
                .attr('x', x + w)
                .attr('y', y)
                .style('font-family', '"Open Sans", sans-serif')
                .style('fill', color)
                .text(term.content);
            
            w += newText.node().getComputedTextLength();
        }

        if (!shouldPrint) {
            newText.remove();
        }

        /*
        const temp = container.append('text')
            .attr('x', x)
            .attr('y', y + h)
            .style('font-family', '"Open Sans", sans-serif')
            .style('fill', color)
            .text(t);

        temp.append('tspan')
            .text(subscript)
            .style('font-size', 12)
            .attr('dx', 5)
            .attr('dy', 5);
*/
    }

    return w;
}

/**
 * a function to construct the text of a label based on the given message
 * @param {*} m - a message prop from the forge spec 
 * @returns a string containing the text for the label
 */
function labelText(m) {
    const data = m.data.tuples().map(x => x.toString());
    const parsed = parseTerms(data);
    return parsedTermsToString(parsed, "");
}

/**
 * a function to center text based on its length
 * @returns a new x value for the text
 */
function centerText() {
    // compute text width     
    const textWidth = this.getComputedTextLength();
    // grab current x value
    const x = d3.select(this).attr('x');
    // re-center text based on textWidth
    return x - (textWidth / 2);
}

/**
 * a function to toggle the visibility of boxes upon mouseclicks
 * @param {*} mouseevent - not used
 * @param {*} timeslot - the timeslot to toggle visible information for
 */
function onMouseClick(mouseevent, timeslot) {
    const ts = timeslot.toString();

    strands.forEach((agent) => {
        const a = agent.toString();
        visible = visibleInformation[ts][a];
        visibleInformation[ts][a] = !visible;
    });
    
    render();
}

/**
 * a function to seperate "complex" data (data using subscripts) from "simple" data
 * @param {*} textArray 
 * @returns 
 */
function filterComplexData(textArray) {

    const simple = [];
    const complex = [];

    parseTerms(textArray).forEach((term) => {

        if ('subscript' in term) {
            complex.push(term);
        } else {
            simple.push(term.content);
        }

    });

    return {simple, complex};
}

/**
 * a helper function for wrapText which finds the last space before the given index in the 
 * given string
 * @param {*} string 
 * @param {*} index 
 * @returns 
 */
function spaceBefore(string, index) {

    let workingString = string;
    let spaceIndex = -1;
    let done = false;
    
    while(!done) {
        let i = workingString.indexOf(" ");
        if (i !== 0 && i !== -1 && i <= index) {
            spaceIndex = i;
            // replace the first space with an X
            workingString = workingString.slice(0, i) + "X" + workingString.slice(i + 1);
        } else {
            done = true;
        }
    }

    return spaceIndex;
}

/**
 * a function to display text over multiple lines
 * @param {*} container 
 * @param {*} text 
 * @param {*} width 
 * @param {*} x 
 * @param {*} y 
 * @param {*} color 
 * @returns the computed height of the resulting text
 */
function wrapText(container, text, width, x, y, color) {

    if (text === "") {
        return 0;
    }

    // this will be replaced if the text ends up overflowing past the given width
    const textElt = container.append('text')
        .attr('x', x)
        .attr('y', y)
        .style('font-family', '"Open Sans", sans-serif')
        .style('fill', color)
        .text(text);

    const w = textElt.node().getComputedTextLength();
    const r = width / w; // w shouldn't be zero because text is non-empty
    if (r < 1) {

        // remove the original text
        textElt.node().remove();

        // math
        const l = text.length;
        let index = Math.round(r * l);
        const spaceIndex = spaceBefore(text, index);
        index = (spaceIndex === -1) ? index : spaceIndex;
        const before = text.slice(0, index);
        const after = text.slice(index);

        // append the "before" text
        container.append('text')
            .attr('x', x) 
            .attr('y', y)
            .style('font-family', '"Open Sans", sans-serif')
            .style('fill', color)
            .text(before);

        // recur on the "after" text
        return LINE_HEIGHT + wrapText(container, after, width, x, y + LINE_HEIGHT, color);
        
    }

    return LINE_HEIGHT;
  
}

/**
 * a function to render the information that appears inside the toggleable boxes
 * @param {*} container 
 * @param {*} info 
 * @param {*} x 
 * @param {*} y 
 * @param {*} color 
 * @returns 
 */
function displayInfo(container, info, x, y, color) {

    let h = 0;

    const {simple, complex} = filterComplexData(info);
    const s = simple.join(" ");

    // render simple data over multiple lines
    h += wrapText(container, s, BOX_WIDTH - 25, x, y, color);

    // render complex data one per line
    let i;
    for (i = 0; i < complex.length; i++) {
        const flattened = flattenParsedTerms([complex[i]]);
        const w = printFlattenedTerms(flattened, container, x, y + h, color, true);

        h+=LINE_HEIGHT;

    }

    return h;
}

/**
 * the main render function of this visualization
 */
function render() {
    // clear the svg
    d3.select(svg).selectAll('*').remove();

    // draw the timeslots
    const t = d3.select(svg)
        .selectAll('timeslot') // giving these shapes a name
        .data(timeslots)
        .join('line')
        .attr('x1', BASE_X)
        .attr('y1', y)
        .attr('x2', BASE_X + ((strands.length - 1) * AGENT_WIDTH))
        .attr('y2', y)
        .attr('stroke', BLACK)
        .attr('fill', 'white')
        .style('stroke-dasharray', ('5, 3'));

    // label the timeslots
    const tLabel = d3.select(svg)
        .selectAll('timeslotLabel')
        .data(timeslots)
        .join('text')
        .on('click', onMouseClick)
        .attr('x', BASE_X - 90)
        .attr('y', y)
        .style('font-family', '"Open Sans", sans-serif')
        .style('cursor', 'pointer')
        .text((t) => t._id);

    // draw the agents
    const a = d3.select(svg)
        .selectAll('agent')
        .data(strands)
        .join('line')
        .attr('stroke', BLUE)
        .style('stroke-width', 10)
        .attr('x1', x)
        .attr('y1', BASE_Y) 
        .attr('x2', x)
        .attr('y2', y(timeslots[timeslots.length - 1]));

    // label the strands with their names
    const aLabel = d3.select(svg)
        .selectAll('agentLabel')
        .data(strands)
        .join('text')
        .attr('x', x)
        .attr('y', BASE_Y - 60)
        .style('font-family', '"Open Sans", sans-serif')
        .text(a => a.toString());

    const aLabel2 = d3.select(svg)
        .selectAll('agentLabel')
        .data(strands)
        .join('text')
        .attr('x', x)
        .attr('y', BASE_Y - 40)
        .style('font-family', '"Open Sans", sans-serif')
        .text((a) => `(agent: ${roles[a.toString()]})`);

    // bind messages to m
    const m = d3.select(svg)
        .selectAll('message')
        .data(messages);

    // join g to m and give it event handlers
    const g = m.join('g');

    // draw lines to represent messages
    g.append('line')            // append a line (becomes a child of g)
        .attr('x1', messageX1)  // the sender
        .attr('y1', messageY1)  // the time sent
        .attr('x2', messageX2)  // the receiver
        .attr('y2', messageY2) // the time received
        .attr('stroke', GREEN)
        .style('stroke-width', 10);

    // functions for rendering arrows 
    const arrowX1 = (m) => messageX1(m) > messageX2(m) ? messageX2(m) + 20 : messageX2(m) - 20;
    const arrowTopY1 = (m) => messageY2(m) + 20;
    const arrowBottomY1 = (m) => messageY2(m) - 20;
    const arrowTopY2 = (m) => messageY2(m) - 3;
    const arrowBottomY2 = (m) => messageY2(m) + 3;

    // forming the top of the arrow
    g.append('line')
        .attr('stroke', GREEN)
        .style('stroke-width', 10)
        .attr('x1', arrowX1)
        .attr('y1', arrowTopY1)
        .attr('x2', messageX2)
        .attr('y2', arrowTopY2);

    // forming the bottom of the arrow
    g.append('line')
        .attr('stroke', GREEN)
        .style('stroke-width', 10)
        .attr('x1', arrowX1)
        .attr('y1', arrowBottomY1)
        .attr('x2', messageX2)
        .attr('y2', arrowBottomY2);

    // for each message...

    messages.forEach(m => {
        let labelX = (x(m.sender) + x(m.receiver)) / 2;
        let labelY = y(m) - 20;

        const data = m.data.tuples().map(x => x.toString());
        const parsed = parseTerms(data);
        const flattened = flattenParsedTerms(parsed);

        const w = printFlattenedTerms(flattened, g, labelX, labelY, BLACK, false);

        const newLabelX = labelX - Math.round((w / 2)); // rounding to make less blurry?

        printFlattenedTerms(flattened, g, newLabelX, labelY, BLACK, true);

    });

    timeslots.forEach((timeslot) => {
        let ts = timeslot.toString();
        strands.forEach((agent) => {
            let a = agent.toString();
            if (visibleInformation[ts][a]) {

                const boxX = x(agent) - (BOX_WIDTH / 2.0);
                const boxY = y(timeslot) + 30;

                // create a group and give it an id specific to this timeslot-agent pair
                const id = ts + a;
                const g = d3.select(svg)
                    .append('g')
                    .attr('id', id)
                    .attr('x', boxX)
                    .attr('y', boxY)
                    .attr('width', BOX_WIDTH)
                    .attr('height', BOX_HEIGHT);
                /*
                const g = d3.select(svg)
                    .append('svg')
                    .attr('id', id)
                    .attr('x', boxX)
                    .attr('y', boxY)
                    .attr('width', BOX_WIDTH)
                    .attr('height', BOX_HEIGHT)
                    .attr('viewBox', `${boxX} ${boxY} ${BOX_WIDTH} ${BOX_HEIGHT}`)
                    .style('overflow', 'scroll')
                    .style('display', 'block');*/
            
                // append the rect
                const r = g.append('rect')
                    .attr('x', boxX)
                    .attr('y', boxY)
                    .attr('width', BOX_WIDTH)
                    .attr('height', BOX_HEIGHT)
                    .attr('rx', 6)
                    .attr('ry', 6)
                    .style('fill', 'white')
                    .style('opacity', .8)
                    .attr('stroke', BLUE)
                    .attr('stroke-width', '3');

                // collect the new information
                let newInfo = [];
                if (learnedInformation[ts] && learnedInformation[ts][a]) {
                    newInfo = learnedInformation[ts][a];
                }

                // collect the generated information
                let generatedInfo = [];
                if (generatedInformation[ts] && generatedInformation[ts][a]) {
                    generatedInfo = generatedInformation[ts][a];
                    // remove any generated info from newInfo
                    generatedInfo.forEach((x) => {
                        const index = newInfo.indexOf(x);
                        if (index > -1) {
                            newInfo.splice(index, 1);
                        }
                    });
                }

                // collect the old information
                let oldInfo = [];
                const sliceIndex = timeslots.indexOf(timeslot);
                const previousTimeslots = timeslots.slice(0, sliceIndex).map(t => t.toString());
                previousTimeslots.forEach((old_ts) => {
                    if (learnedInformation[old_ts] && learnedInformation[old_ts][a]) {
                        oldInfo = oldInfo.concat(learnedInformation[old_ts][a]);
                    }
                });

                let h = 0;

                h += displayInfo(g, generatedInfo, boxX + 10, boxY + LINE_HEIGHT, BLUE);

                h += displayInfo(g, newInfo, boxX + 10, boxY + LINE_HEIGHT + h, RED);

                h += displayInfo(g, oldInfo, boxX + 10, boxY + LINE_HEIGHT + h, BLACK);

                // TODO: calculate the resulting height and offset the next group of information
                
            } else {
                // remove the group if this timeslot is not supposed to be visible
                d3.select('#' + ts + a).remove(); 
            }
        });
    });

}

render();

