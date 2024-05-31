/*
  Script for Binary Search example

  ASSUMES: all sigs as written in binarysearch.frg
*/

////////////////////////////////////////////////////////////

/** Helper to extract the atom from a singleton 
 *  Note: this does not convert to the atom's id(); may get wrapping [] */
function atomOf(expr, row, col) {
    if(!('empty' in expr) || !('tuples' in expr)) throw `atomOf: expected expression, got: ${expr}`
    if(expr.length < row-1) throw `atomOf: number of rows insufficient: ${expr}`
    if(!expr.empty()) {
      if(expr.tuples()[row] === undefined)
        throw `atomOf: got undefined for ${row}th tuple in ${expr.tuples()[row]}`
      if(expr.tuples()[row].atoms()[col] === undefined)
        throw `atomOf: got undefined for ${col}th column in ${row}th tuple of ${expr.tuples()[row]}`
      return expr.tuples()[row].atoms()[col]
    }
    return 'none'
}
/** Helper that returns a filter-function that filters for a specific set of atom IDs */
function atomIdIn(idArr) {
    return atomObj => atomObj.id() in idArr
}

////////////////////////////////////////////////////////////

// Should only be one array
const arrays = IntArray.tuples().map(ltup => atomOf(ltup, 0, 0))
const arrayElements = (arrays[0]).join(elements).tuples()
const states = SearchState.tuples().map(ltup => atomOf(ltup, 0, 0))
const targetElement = atomOf(SearchState.join(target), 0, 0)

const arrayGridConfig = {
    // Absolute location in parent (here, of the stage itself)
    grid_location: { x:10, y:150},
    // How large is each cell?
    cell_size: {x_size:80,y_size:50},
    // How many rows and columns?
    grid_dimensions: {
        // One row for each state
        y_size:states.length,
        // One column for every entry in the array (plus state label)
        x_size:arrayElements.length + 1}}

const arrayGrid = new Grid(arrayGridConfig)

// Populate a row in the table for each state
states.forEach((stateVal, stateIdx) => {
    // Populate label
    arrayGrid.add({x: 0, y: stateIdx}, new TextBox({text: `${stateVal.id().replace('SearchState','S')}`}));
    // Identify low/high 
    const lowAtom = atomOf(stateVal.join(low), 0, 0)
    const highAtom = atomOf(stateVal.join(high), 0, 0)
    // Populate array
    arrayElements.forEach((eVal, eIdx) => {
        const indexAtom = atomOf(eVal, 0, 0)
        const valueAtom = atomOf(eVal, 0, 1)
        const labels = `${lowAtom.id() == indexAtom ? 'L' : ''}${highAtom.id() == indexAtom ? 'H' : ''}`
        arrayGrid.add({x: parseInt(indexAtom.id())+1, y: stateIdx}, 
            new TextBox({text: `${valueAtom} ${labels}`, color: valueAtom.id() == targetElement.id() ? 'red' : 'black'}))
    })
})

// Finally, render everything
const stage = new Stage() 
stage.add(arrayGrid)
stage.render(svg)







