function loadInput(name, defaultValue) {
    var value = url('?' + name);
    var input = document.getElementById(name);
    input.value = value !== null ? value : defaultValue;
}

loadInput('bayes-elo-0', '-1.5');
loadInput('bayes-elo-1', '4.5');
loadInput('alpha', '0.05');
loadInput('beta', '0.05');
loadInput('draw-elo', '250');

var data = [];

var bayesElo0 = null;
var bayesElo1 = null;
var alpha = null;
var beta = null;
var drawElo = null;

var eloStart = null;
var eloEnd = null;

var passProbChart = d3.select('#pass-prob-chart');
var numGamesChart = d3.select('#num-games-chart');

var margin = {top: 10, right: 50, bottom: 60, left: 60};
var width = 570 - margin.left - margin.right;
var height = 480 - margin.top - margin.bottom;

var x = d3.scale.linear().range([0, width]);
var probScale = d3.scale.linear().domain([0, 1]).range([height, 0]);
var numGameScale = d3.scale.linear().range([height, 0]);


buildChart(passProbChart, 'Pass Probability');
buildChart(numGamesChart, 'Expected Number of Games');

function applyAxis(selector, axis) {
    var orientation = axis.orient();
    var axisType = orientation == "bottom" ? "x" : "y";

    d3.select(selector + " .plot-area ." + axisType + ".axis").call(axis);
    drawGridTicks(selector, axisType, axis.scale());
}

function drawGridTicks(selector, axisType, scale) {
    var ticks = scale.ticks(5);
    var minorTicks = [];
    for (var i = 0; i < ticks.length - 1; i++) {
        minorTicks.push((ticks[i] + ticks[i+1]) / 2);
    }

    ticks = ticks.concat(minorTicks);
    ticks.sort(function(a, b) { return a - b; });

    var g = d3.select(selector + " .grid ." + axisType);

    var selection = g.selectAll("line").data(ticks, function(d) { return d; });
    selection.exit().remove();
    selection.enter().append("line");
    selection
        .classed("minor", function(d, i) { return i % 2; })
        .attr("x1", function(d) { return axisType == "x" ? scale(d) : 0; })
        .attr("y1", function(d) { return axisType == "x" ? 0 : scale(d); })
        .attr("x2", function(d) { return axisType == "x" ? scale(d) : width; })
        .attr("y2", function(d) { return axisType == "x" ? height : scale(d); });
}

var probAxis = d3.svg.axis()
    .scale(probScale)
    .tickFormat(d3.format("0.1f"))
    .ticks(5)
    .orient("left");

applyAxis("#pass-prob-chart", probAxis);

displayData();

function showTooltips() {
    d3.selectAll(".chart-tooltip").style("display", null);
}

function hideTooltips() {
    d3.selectAll(".chart-tooltip").style("display", "none");
}

function updateTooltips() {
    var x0 = x.invert(d3.mouse(this)[0]);
    var i = Math.round((x0 - eloStart) * 100);
    var d = data[i];

    var tooltip = passProbChart.select(".chart-tooltip");

    tooltip.select("circle")
        .attr("transform", "translate(" + x(d.elo) + "," + probScale(d.passProb) + ")");
    tooltip.select("text")
        .attr("transform", "translate(" + x(d.elo) + "," + probScale(d.passProb) + ")")
        .text("(" + d.elo + ", " + d3.format("0.4f")(d.passProb) + ")" );

    tooltip = numGamesChart.select(".chart-tooltip");

    tooltip.select("circle")
        .attr("transform", "translate(" + x(d.elo) + "," + numGameScale(d.expNumGames) + ")");
    tooltip.select("text")
        .attr("transform", "translate(" + x(d.elo) + "," + numGameScale(d.expNumGames) + ")")
        .text("(" + d.elo + ", " + d3.format("0.1f")(d.expNumGames / 1000.0) + "k)" );
}

function buildChart(chart, yLabel) {
    chart.attr("width", width + margin.left + margin.right)
         .attr("height", height + margin.top + margin.bottom);

    var grid = chart.append("g")
        .attr("class", "grid")
        .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

    grid.append("g")
        .attr("class", "grid-background")
        .append("rect")
        .attr("width", width)
        .attr("height", height);

    grid.append("g")
        .attr("class", "x grid");

    grid.append("g")
        .attr("class", "y grid");

    var plotArea = chart.append("g")
        .attr("class", "plot-area")
        .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

    plotArea.append("g")
        .attr("class", "x axis")
        .attr("transform", "translate(0," + height + ")");

    plotArea.append("g")
        .attr("class", "y axis");

    plotArea.append("path")
        .attr("class", "line");

    drawXAxisLabel(chart, 'Elo');
    drawYAxisLabel(chart, yLabel);

    var tooltip = plotArea.append("g")
        .attr("class", "chart-tooltip")
        .style("display", "none");

    tooltip.append("circle").attr("r", 3);
    tooltip.append("text").attr("dy", 15).attr("dx", 5)
        .style("fill", "black")
        .style("stroke", "none")
        .style("font", "10px sans-serif");

    plotArea.append("rect")
        .attr("width", width)
        .attr("height", height)
        .style("fill", "none") //style in css
        .style("pointer-events", "all")
        .on("mouseover", showTooltips)
        .on("mouseout", hideTooltips)
        .on("mousemove", updateTooltips);

}

function drawXAxisLabel(chart, text) {
    chart.append("text")
         .attr("text-anchor", "middle")
         .attr("x", margin.left + width / 2)
         .attr("y", height + margin.top)
         .attr("dy", "3em")
         .text(text);
}

function drawYAxisLabel(chart, text) {
    chart.append("text")
        .attr("text-anchor", "middle")
        .attr("y", margin.left)
        .attr("x", - margin.top - (height / 2))
        .attr("dy", "-3em")
        .attr("transform", "rotate(-90)")
        .text(text);
}

function setArgs() {
    bayesElo0 = parseFloat(document.getElementById('bayes-elo-0').value);
    bayesElo1 = parseFloat(document.getElementById('bayes-elo-1').value);
    alpha = parseFloat(document.getElementById('alpha').value);
    beta = parseFloat(document.getElementById('beta').value);
    drawElo = parseFloat(document.getElementById('draw-elo').value);
}

function setEloDomain() {
    var d = bayesElo1 - bayesElo0;
    eloStart = Math.floor(bayesElo0 - d );
    eloEnd = Math.ceil(bayesElo1 + d );
    eloStart = -4;
    eloEnd = 8;
}

function displayData() {
    setArgs();
    setEloDomain();

    x.domain([eloStart, eloEnd]);
    var xAxis = d3.svg.axis().scale(x).ticks(5).orient("bottom");

    applyAxis("#pass-prob-chart", xAxis);
    applyAxis("#num-games-chart", xAxis);

    data.length = 0;
    var sprt = new Sprt(alpha, beta, bayesElo0, bayesElo1, drawElo);

    var numGameBound = 0;
    for (var i = eloStart * 100; i <= eloEnd * 100; i += 1) {
        var elo = i / 100;
        var bayesElo = elo / scale(drawElo);
        var expNumGames = sprt.characteristics(bayesElo)[1];
        data.push({ 
            elo: elo,
            bayesElo: bayesElo,
            passProb: sprt.characteristics(bayesElo)[0],
            expNumGames: expNumGames
        });
        numGameBound = Math.max(numGameBound, expNumGames);
    }

    numGameBound = 10000 * Math.ceil(numGameBound / 10000);

    // numGameScale.domain([0, numGameBound]);
    numGameScale.domain([0, 250000]);

    var numGameAxis = d3.svg.axis()
        .scale(numGameScale)
        .ticks(5)
        .tickFormat(function(d) { return d/1000 + 'k'; })
        .orient("left");

    applyAxis("#num-games-chart", numGameAxis);

    var lineX = function(d) { return x(d.elo); };

    plotLine(d3.select("#pass-prob-chart .plot-area"), data,
            lineX, function(d) { return probScale(d.passProb); });
    plotLine(d3.select("#num-games-chart .plot-area"), data,
            lineX, function(d) { return numGameScale(d.expNumGames); });
}

function plotLine(plotArea, data, x, y) {
    var line = d3.svg.line()
                     .interpolate("cardinal")
                     .x(x)
                     .y(y);

    plotArea.datum(data).select(".line").attr("d", line);
}

document.getElementById('parameters').addEventListener('submit', function (e) {
    displayData();
    e.preventDefault();
});


