function jumpPrevCycle() {
  while (undo()) {
    if (!inReactFlush()) return true;
  }
}

function jumpNextCycle() {
  while (log.length) {
    doNext();
    if (!inReactFlush()) return true;
  }
}

function inReactFlush() {
  return Object.keys(nodes).some(function (id) {
    var node = nodes[id];
    return node.running || node.invalidated || node.changed;
  });
}

// Save the original log before automatically stepping in on page load
var origLog = log.slice(0);
var componentLabels = __COMPONENT_LABELS__;
var filterStack = [];

function filterOnSelection() {
  if (filterStack.length) return;

  var selectedIds = selectedNodeIds();
  if (!selectedIds.length) return;

  var labels = selectedIds.map(function (id) {
    return componentLabels[id];
  });

  filterStack.push({ prevTotalSteps: totalSteps });

  var messageFilter = function (message) {
    return labels.some(function (label) {
      return componentLabels[message.id] === label;
    });
  };

  var filteredLog = origLog.filter(messageFilter);

  var nextMessage;
  for (var i = 0; i < log.length; i++) {
    var message = log[i];
    if (messageFilter(message)) {
      nextMessage = message;
      break;
    }
  }

  initializeLog(filteredLog);
  jumpToMessage(nextMessage);
  selectNodes(selectedIds);
}

function resetFilter() {
  var nextMessage = log[0];
  var selectedIds = selectedNodeIds();

  while (filterStack.length) {
    var currentFilter = filterStack.pop();
    initializeLog(origLog.slice(0), currentFilter.prevTotalSteps);
    jumpToMessage(nextMessage);
  }

  selectNodes(selectedIds);
}

function selectedNodeIds() {
  var selectedNodes = d3.select('#nodes').selectAll('.fixed').data();

  var idsByIndex = Object.keys(nodes).reduce(function (obj, id) {
    var node = nodes[id];
    if (node.index != null) obj[node.index] = id;
    return obj;
  }, {});

  return selectedNodes.map(function (node) {
    return idsByIndex[node.index];
  });
}

function initializeLog(newLog, newTotalSteps) {
  log = newLog;
  nodes = {};
  nodeList = [];
  links = [];
  undoStack = [];
  step = 0;
  executeBeforeNextCommand = [];
  totalSteps = (newTotalSteps != null) ? newTotalSteps : countSteps();

  // hide end message triggered by countSteps()
  $('#ended').stop(true).hide();

  doNext();

  while (undoStack.length) {
    undoStack.pop();
  }
}

function jumpToMessage(message) {
  while (log.length) {
    if (log[0] === message) {
      break;
    }
    doNext();
  }
}

function selectNodes(nodeIds) {
  var allNodes = d3.select('#nodes').selectAll('.node')[0];
  nodeIds.map(function (id) {
    var node = nodes[id];
    d3.select(allNodes[node.index]).classed('fixed', true);
  });
}

$(function () {
  var html = '<div id="controls">' +
    '<button id="prev-cycle-btn">Prev Cycle</button>' +
    '<button id="next-cycle-btn">Next Cycle</button>' +
    '<button id="filter-selection-btn">Filter on Selection</button>' +
    '<button id="reset-filter-btn">Reset Filter</button>' +
    '</div>';

  var style = '<style type="text/css">' +
    '#instructions, #ended { top: 0px; }' +
    '#controls { position: relative; margin-top: 15px; z-index: 1; }' +
    '#controls button { margin: 2px; }' +
    '</style>';

  $('body').prepend(html);
  $('head').append(style);

  $('#prev-cycle-btn').click(jumpPrevCycle);
  $('#next-cycle-btn').click(jumpNextCycle);
  $('#filter-selection-btn').click(filterOnSelection);
  $('#reset-filter-btn').click(resetFilter);
});
