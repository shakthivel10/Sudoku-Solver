
var s = new Array(9);

for (var i = 0; i < 9; i++) {
    s[i] = []
}


for (var i = 0; i < 9; i++) {
    for (var j = 0; j < 9; j++) {
        s[i][j] = document.getElementById('element-' + i + j).innerHTML
    }
}


