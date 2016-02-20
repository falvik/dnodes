//var sessionId;
var socket;
var connected = false;
var tasksId = 0;
var tasks = {};
var sent = {};

function connect(){
    socket = new WebSocket("ws://" + window.location.host
     + "/websocket");
    socket.onopen = function(){
        connected = true;
        doSend();
    };
    socket.onclose = function(){
        connected = false;
        sent = {};
        window.setTimeout(function(){connect();}, 1000);
    }
    socket.onmessage = function(event){
        var data = event.data;
        var json = JSON.parse(data);
        doTask(json);
    };
    socket.onerror = function(){
        connected = false;
        sent = {};
        window.setTimeout(function(){connect();}, 1000);
    };
}

function doTask(json){
    if(json.id === undefined){
        addTask({"command":"nodes_info"}, function(nodes){repaint(nodes);});
    } else {
        var task = sent[json.id];
        task.callback(json.data);
        delete sent[json.id];
    }
}

function addTask(request, callback){
    var id = tasksId++;
    request.id = id;
    var task;
    if(callback){
        task = {"request":request, "callback":callback};
    } else {
        task = {"request":request};
    }
    tasks[id] = task;
    if(connected){
        doSend();
    }
}

function doSend(){
    for(var id in tasks){
        var err = false;
        var task = tasks[id];
        try{
            socket.send(JSON.stringify(task.request));
        } catch(ex){
            err = true;
        }
        if(!err){
            delete tasks[id];
            if(task.callback){
                sent[id] = task;
            }
        }
    }
}

function repaint(json){
    var svg = document.getElementById("svg");
    while (svg.lastChild) {svg.removeChild(svg.lastChild);}
    var k = 150;
    var x0 = 2.1 * k;
    var y0 = 1.1 * k;
    var rx = 10;
    var ry = 20;
    var x_indent = 3;
    var y_indent = 14;
    var font_size = 12;

    var rx_text = rx;
    // Find maximum text length
    for(var a = 0; a < json.length; a++){
        var g = document.createElementNS("http://www.w3.org/2000/svg", 'g');
        var text = document.createElementNS("http://www.w3.org/2000/svg", 'text');
        text.setAttribute("font-size",font_size);
        text.setAttribute("hidden", "true");
        text.textContent = json[a].node_id;

        g.appendChild(text);
        svg.appendChild(g);

        var text_width = text.getComputedTextLength() + 6;
        if(text_width > rx_text)
            rx_text = text_width;
    }

    // Draw links between nodes.
    rx = rx_text;
    for(var a = 0; a < json.length; a++){
        var z = 2 * Math.PI * a / json.length;
        var x = x0 + k*Math.sin(z)*2;
        var y = y0 - k*Math.cos(z);
        json[a].x=x;
        json[a].y=y;
        for(var b = 0; b < a; b++){
            var id = json[b].node_id;
            if(json[a].links.includes(id)){
                //рисуем линию
                var x1 = json[a].x + rx/2;
                var y1 = json[a].y + ry/2;
                var x2 = json[b].x + rx/2;
                var y2 = json[b].y + ry/2;
                var cx1 = (x0+rx/2 + x1)/2;
                var cy1 = (y0 + y1)/2;
                var cx2 = (x0+rx/2 + x2)/2;
                var cy2 = (y0 + y2)/2;
                if(a == (b+1) || (b==0 && a == json.length-1)){
                    cx1 = x1;
                    cy1 = y1;
                    cx2 = x2;
                    cy2 = y2;
                }
                var path = document.createElementNS("http://www.w3.org/2000/svg", 'path');
                path.setAttribute("d","M"
                          +  x1 + " " +  y1 + " C "
                          + cx1 + " " + cy1 + " " +
                            cx2 + " " + cy2 + " " +
                             x2 + " " +  y2
                 );
                path.setAttribute("style","stroke:rgb(0,0,0);stroke-width:1;fill:none");
                svg.appendChild(path);
            }
        }
    }
    // Draw nodes and print their id's.
    for(var a = 0; a < json.length; a++){
        var x = json[a].x;
        var y = json[a].y;
        var g = document.createElementNS("http://www.w3.org/2000/svg", 'g');
        var rect = document.createElementNS("http://www.w3.org/2000/svg", 'rect');
        rect.setAttribute("width", rx);
        rect.setAttribute("height", ry);
        rect.setAttribute("x", x);
        rect.setAttribute("y", y);
        rect.setAttribute("style","fill:rgb(255,255,255);stroke-width:1;stroke:rgb(0,0,0)");
        g.appendChild(rect);
        var text = document.createElementNS("http://www.w3.org/2000/svg", 'text');
        text.setAttribute("x", x + x_indent);
        text.setAttribute("y", y + y_indent);
        text.setAttribute("font-size",font_size);
        text.textContent = json[a].node_id;
        g.appendChild(text);
        svg.appendChild(g);

        //console.log(a + ":" + text.getComputedTextLength());
    }
}

function newNode(){
    addTask({"command":"newNode"});
}

function deleteNode(){
    var deleteNodeId = $("#deleteNodeId").val();
    addTask({"command":"deleteNode", "nodeId": deleteNodeId});
}

function addLink(){
    var node1 = $("#addLink1").val();
    var node2 = $("#addLink2").val();
    addTask({"command":"addLink", "nodeId1":node1, "nodeId2":node2});
}

function deleteLink(){
    var node1 = $("#deleteLink1").val();
    var node2 = $("#deleteLink2").val();
    addTask({"command":"deleteLink", "nodeId1":node1, "nodeId2":node2});
}

function start(){
    connect();
    addTask({"command":"nodes_info"}, function(nodes){repaint(nodes);});
}