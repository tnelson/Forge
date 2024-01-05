const baseX = 150;
const baseY = 100;
const timeslotHeight = 60;
const nameWidth = 200;
const boxHeight = 130;
const boxWidth = 200;

const timeslots = Timeslot.atoms(true);
const names = name.atoms(true);
const messages = Timeslot.atoms(true);

const dataMessageMap = {};
const pubKeyMap = {};
const privKeyMap = {};

data.tuples().forEach((tuple) => {
    let m = tuple.atoms()[0];
    let d = tuple.atoms()[1].toString();
    dataMessageMap[d] = m;
});

KeyPairs0.owners.tuples().forEach(x => {
    let atoms = x.atoms();
    let key = atoms[0].toString();
    let owner = atoms[1].toString();
    privKeyMap[key] = owner;
});

KeyPairs0.pairs.tuples().forEach(x => {
    let atoms = x.atoms();
    let private = atoms[0].toString();
    let public = atoms[1].toString();
    let owner = privKeyMap[private]; 
    pubKeyMap[public] = owner;
});

function getTimeSlotsBefore(timeslot) {
    const sliceIndex = timeslots.indexOf(timeslot);
    return timeslots.slice(0, sliceIndex).map(t => t.toString());
}

const x = name => baseX + (names.indexOf(name) * nameWidth);
const y = timeslot => baseY + (timeslots.indexOf(timeslot) * timeslotHeight);
const messageX1 = m => x(m.sender.agent);
const messageX2 = m => x(m.receiver.agent);
const messageY = m => y(m); 

function labelX() {
    const l = d3.select(this.parentNode).select('line');
    const labelX1 = parseInt(l.attr('x1'));
    const labelX2 = parseInt(l.attr('x2'));
    return (labelX1 + labelX2) / 2.0;
}

function labelY() {
    const l = d3.select(this.parentNode).select('line');
    return parseInt(l.attr('y1')) - 20;
}

function labelText(m) {
    const pt = m.data.tuples().map(tuple => {
        let datum = tuple.atoms()[0].plaintext.toString();
        if (pubKeyMap[datum]) {
            return `pubK${pubKeyMap[datum]}`;
        } else if (privKeyMap[datum]) {
            return `privK${privKeyMap[datum]}`;
        } else {
            return datum;
        }
    });
    const ptString = pt;
    return `{${ptString}}`;
}

function subscriptText(m) {
    let pubKey = m.data.encryptionKey.toString();
    let owner = pubKeyMap[pubKey];
    return `pubK${owner}`;
}

function centerText() {
    const textWidth = this.getComputedTextLength();
    const x = d3.select(this).attr('x');
    return x - (textWidth / 2);
}

d3.select(svg).selectAll('*').remove();

const tLabel = d3.select(svg)
    .selectAll('timeslotLabel')
    .data(timeslots)
    .join('text')
    .attr('x', baseX - 90)
    .attr('y', y)
    .text((t) => t._id);

const a = d3.select(svg)
        .selectAll('name')
        .data(names)
        .join('line')
        .style('stroke-width', 5)
        .attr('stroke', '#81A4CD')
        .attr('x1', x)
        .attr('y1', baseY) 
        .attr('x2', x)
        .attr('y2', y(timeslots[timeslots.length - 1]));

const aLabel = d3.select(svg)
    .selectAll('nameLabel')
    .data(names)
    .join('text')
    .attr('x', x)
    .attr('y', baseY - 40)
    .text((a) => a._id);

const m = d3.select(svg)
    .selectAll('message')
    .data(messages);

const g = m.join('g');

g.append('line')            // append a line (becomes a child of g)
    .attr('x1', messageX1)  // the sender
    .attr('y1', messageY)   // the time sent
    .attr('x2', messageX2)  // the receiver
    .attr('y2', messageY)   // the time received
    .style('stroke-width', 5)
    .attr('stroke', '#054A91');

const arrowX1 = (m) => messageX1(m) > messageX2(m) ? messageX2(m) + 20 : messageX2(m) - 20;
const arrowTopY1 = (m) => messageY(m) + 20;
const arrowBottomY1 = (m) => messageY(m) - 20;
const arrowTopY2 = (m) => messageY(m) - 3;
const arrowBottomY2 = (m) => messageY(m) + 3;

g.append('line')
    .style('stroke-width', 5)
    .attr('stroke', '#054A91')
    .attr('x1', arrowX1)
    .attr('y1', arrowTopY1)
    .attr('x2', messageX2)
    .attr('y2', arrowTopY2);

g.append('line')
    .style('stroke-width', 5)
    .attr('stroke', '#054A91')
    .attr('x1', arrowX1)
    .attr('y1', arrowBottomY1)
    .attr('x2', messageX2)
    .attr('y2', arrowBottomY2);

const label = g.append('text')
    .attr('x', labelX)
    .attr('y', labelY)
    .text(labelText);

label.append('tspan')
        .text(subscriptText)
        .style('font-size', 12)
        .attr('dx', 5)
        .attr('dy', 5);

label.attr('x', centerText);