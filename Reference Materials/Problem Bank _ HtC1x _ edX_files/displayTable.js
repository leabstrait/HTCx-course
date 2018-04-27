// produce sub-section and problem summary tables

//
// The naming convention is that
//     makeXXX   builds structure
//     createXxx builds structure and adds it to its first argument (effectful)


// !!! this probably goes away?
var MODULE_ORDER = [ 'BSL', 'HtDF', 'HtDD', 'HtDW', 'Compound',

		     'Self-Ref', 'Ref', 'Naturals', 'Helpers', 'BSTs', 'Mutual-Ref', 

		     '2-One-Of', 'Local', 
		     'Abstraction', 
		     
		     'Genrec', 'Search', 'Accumulators','Graphs'];

var CURRENT_MODULE = 'Graphs'; // !!!

function moduleIsReleased(module) {
    return MODULE_ORDER.indexOf(module) <= MODULE_ORDER.indexOf(CURRENT_MODULE);
}



// Global Constants and Functions for Creating URLs----------------------------

var IMAGE_FILE_BASE            = "https://edge.edx.org/c4x/UBCx/CPSC1101/asset/";
var STARTER_FILE_BASE          = "https://s3.amazonaws.com/edx-course-spdx-kiczales/HTC/";
var SOLUTION_FILE_BASE         = "https://s3.amazonaws.com/edx-course-spdx-kiczales/HTC/";
var NOTES_FILE_BASE            = "/static/";
var IN_VIDEO_QUIZZES_FILE_BASE = "/static/";
 
var LECTURE_VIDEO_BASE         = "../lecture/";
var DOWNLOAD_ON_CLICK          = "?response-content-type=application%2Foctet-stream&response-content-disposition=attachment";

function createImageFileURL      (filename)         { return IMAGE_FILE_BASE + filename; }

function createLectureVideoURL (lecture)            { return downcase(LECTURE_VIDEO_BASE + 'view?lecture_id='         + lecture.videoNum);}
function createTranscriptURL   (lecture)            { return downcase(LECTURE_VIDEO_BASE + 'subtitles?q='             +  lecture.videoNum + '_en&format=txt'); }
function createSRTURL          (lecture)            { return downcase(LECTURE_VIDEO_BASE + 'subtitles?q='             +  lecture.videoNum + '_en&format=srt'); }
function createVideoMP4URL     (lecture)            { return downcase(LECTURE_VIDEO_BASE + 'download.mp4?lecture_id=' + lecture.videoNum); }


function createStarterFileURL  (module, filename)   { return STARTER_FILE_BASE          + downcase(filename); }
function createSolutionFileURL (module, filename)   { return SOLUTION_FILE_BASE         + downcase(filename); }
function createNotesURL        (lecture)            { return downcase(NOTES_FILE_BASE            + lecture.module + "/" + lecture.notes); }
function createQuizzesURL      (lecture)            { return downcase(IN_VIDEO_QUIZZES_FILE_BASE + lecture.module + "-" + lecture.label + '.pdf'); }

function downcase (str) { return str.toLowerCase(); }  //just makes above more compact

function getLectures() { return LECTURES_PROBLEMS.lectures; }
function getProblems() { return LECTURES_PROBLEMS.problems; }




// Global Settings for Look and Feel of Tables---------------------------------

function setTableStyleAndProperties(table) {
    table.border = "1";
    table.cellpadding = "5";
    table.width = "90%"; 
    
    //Below are CSS Style attributes set to match Module1 Table
    table.style.border = "1px solid rgb(0, 0, 0)";
    table.style.padding = "5px"; 
    table.style.textAlign = "left";
    table.style.horizontalAlign = "left"; 
    table.style.verticalAlign = "top";
    table.style.maxWidth = "90%";
}



// make external entry points

function displayNotesLink(ID, module) {
    document.getElementById(ID).innerHTML = ''; 
    document.getElementById(ID).appendChild(document.createTextNode("!!! Notes link will go here!!!"));
}

function displayProblemTable(id, moduleFilter, lectureFilter, kindFilter) {        
    outputTable(makeProblemTable(filterProblems(getProblems(), moduleFilter, lectureFilter, kindFilter)), id);
}

function displayProblemTable2(id, moduleFilter, kindFilter) {
    outputTable(makeProblemTable2(filterProblems(getProblems(), moduleFilter, 'all', kindFilter)), id);
}

function displayProblemTable3(id, problemNames) {
    outputTable(makeProblemTable2(filterProblems3(getProblems(), problemNames)), id);
}



function displayLectureTable(ID, moduleFilter){
    outputTable(makeLectureTable(filterLectures(getLectures(), moduleFilter)), ID);
}

function displayLectureTable2(ID, moduleFilter){
    outputTable(makeLectureTable2(filterLectures(getLectures(), moduleFilter)), ID);
}


// internal from here on out

function makeProblemTable(problems) { 
    var table = makeTable();

    addTableHead(table, ['#', 'Description', 'Length', 'Difficulty', 'Code Files', 'Requires<br>Lecture']);
    addTableBody(table, problems, [createProblemNumCell, 
				   createDescriptionCell,
				   createProblemLengthCell,
				   createDifficultyCell,
				   createCodeFilesCell,
				   createLectureCell]);

    return table;
}

function makeProblemTable2(problems) { 
    var table = makeTable();

    addTableHead(table, ['Description', 'Length', 'Difficulty']);
    addTableBody(table, problems, [createCombinedDescriptionCell, 
				   createProblemLengthCell,
				   createDifficultyCell]);

    return table;
}



function makeLectureTable(lectures){
    var table = makeTable();

    addTableHead(table, ['Topic', 'Length <br> (mm:ss)', 'Starter File(s)' ]);
    addTableBody(table, lectures, [createTopicCell,
				   createLectureLengthCell,
				   createStarterFileCell]);

    return table;

}


function makeLectureTable2(lectures){
    var table = makeTable();

    addTableHead(table, ['Section (video length)   Starter File(s) <br>' +
			 'Description <br>' +
			 'Problems in this section.']);
    addTableBody(table, lectures, [createLectureSummaryCell]);

    return table;

}








// Simple cells

function createProblemNumCell(row, problem) {
    var cell = row.insertCell(-1);
    var pNum = makeProblemNum(problem);

    cell.align = "center";

    cell.appendChild(document.createTextNode(pNum));
}

function createDescriptionCell(row, problem) {
    var cell = row.insertCell(-1);
    var desc = problem.description;

    cell.appendChild(document.createTextNode(desc));    
}

function createProblemLengthCell(row, problem) {
    var cell = row.insertCell(-1);

    cell.align = "center";
    cell.style.columnWidth = "40px";

    cell.appendChild(document.createTextNode(problem.duration + ' min.'));
}

function createLectureLengthCell(row, lecture) {
    var cell = row.insertCell(-1);

    cell.align = "center";
    cell.style.columnWidth = "40px";

    cell.appendChild(document.createTextNode(lecture.length + ' min.'));
}

function createLectureCell(row, problem) {
    var cell    = row.insertCell(-1);
    var lecture = problem.lecture;

    cell.align = "center";

    cell.appendChild(document.createTextNode(lecture));
}

function createTopicCell(row, lecture){
    var cell = row.insertCell(-1);
    var title = lecture.title;

    cell.appendChild(document.createTextNode(title)); 
}


function makeProblemNum(problem) {
    return problem.module + ' ' + PorL(problem.kind) + problem.setNumber;
}
	
function PorL(kind) {
    if (kind === 'Practice') {
        return 'P';
    } else if (kind === 'Lecture') {
        return 'L';
    }
}




// more complex cells

function createLectureSummaryCell(row, lecture) {   
    var cell = row.insertCell(-1);
    var div = document.createElement("div");
    var title = lecture.title;
    var problems = filterProblems(getProblems(), lecture.module, lecture.label, 'all');
    
    div.appendChild(createElementFromHTML('<b>' + title + '(' + lecture.length + ')</b>' +   lecture.starterFile + '<br>' +
 					  lecture.description + '<br><br>'));
    
    for (var i = 0; i < problems.length; i++) {
	var problem = problems[i];
	div.appendChild(createElementFromHTML(problem.name + '&nbsp;'));
	div.appendChild(makeDifficulty(problem));
    }

    cell.appendChild(div);

 }


function addProblemLine(cell, problem) {
    //var p = document.createElement('p');

    cell.appendChild(document.createTextNode(problem.name));
    
    //cell.appendChild(p);
}



function createCombinedDescriptionCell(row, problem) {    
    var cell         = row.insertCell(-1);

    var num  = makeProblemNum(problem);
    var name = problem.name;
    var desc = problem.description;

    cell.appendChild(createElementFromHTML('<b>' + num + ' - ' + name + '</b><br><br>' + desc));

    addFileLinks(cell, problem);
}


function createDifficultyCell(row,problem) {
    var cell = row.insertCell(-1);    

    cell.align = "center";
    cell.style.columnWidth = "40px";
    
    cell.appendChild(makeDifficulty(problem));
 }

function makeDifficulty(problem) {
    var img = document.createElement('img');

    if (problem.difficulty === "Green") {
	img.title = "easy";
        img.src = createImageFileURL("green_circle.png");
    } else if (problem.difficulty === "Blue") {
	img.title = "medium";
        img.src = createImageFileURL("blue_square.png");
    } else {
	img.title = "hard";
        img.src = createImageFileURL("black_diamond.png");
    }
    
    return img;
}


function createCodeFilesCell(row, problem) {
    var cell = row.insertCell(-1);
    cell.align = "left";

    addFileLinks(cell, problem.module, problem.codeFiles);
}

function createStarterFileCell(row, lecture) {
    var cell = row.insertCell(-1);
    cell.align = "left";

    addFileLinks(cell, lecture.module, lecture.starterFile);
}


function addFileLinks(cell, module, files) {
    if (files === '') {
        cell.appendChild(createElementFromHTML('<i>No starter file.</i>'));
    }

    else if ( typeof files === "string" ) {
	addFileLink(cell, module, files);
    }
	
    else if (Array.isArray(files)) {
	for(var i=0, len=files.length; i < len; i++) {
	    addFileLink(cell, module, files[i]);
	    if(i < len - 1) {
		cell.appendChild(document.createElement('br'));
	    }
	}
    }
}

function addFileLink(cell, module, file) {
    var link = document.createElement('a');

    link.appendChild(document.createTextNode(file));
    link.title = file;
    link.href = createSolutionFileURL(module, file);
    
    cell.appendChild(link);
}






// Global Functions -----------------------------------------------------------


function filterProblems(problems, moduleFilter, lectureFilter, kindFilter) { 
    var filteredProblems = [];

    for (var i = 0; i < problems.length; i++) {
	var problem = problems[i];
	var module  = problem.module;
	var lecture = problem.lecture;
	var kind    = problem.kind;

        if ( (   (moduleFilter  === module)  || (moduleFilter  === 'all'))
             && ((lectureFilter === lecture) || (lectureFilter === 'all'))
             && ((kindFilter    === kind)    || (kindFilter    === 'all'))
	     && moduleIsReleased(module)
	   )
	{
	    filteredProblems.push(problem);
	}
    }

    return filteredProblems;
}


function filterProblems3(problems, problemNames) { 
    var filteredProblems = [];

    for (var i = 0; i < problems.length; i++) {
	var problem = problems[i];
	var name    = problem.name;

        if ( problemNames.indexOf(name) > -1 )
	{
	    filteredProblems.push(problem);
	}
    }

    return filteredProblems;
}

function filterLectures(lectures, moduleFilter) {

    var filteredLectures = []; 

    for (var i = 0; i < lectures.length; i++) {
        var lecture = lectures[i];
        var module  = lecture.module;

        if ( ((moduleFilter   === module)   || (moduleFilter === 'all'))
	     && moduleIsReleased(module)) {

            filteredLectures.push(lecture);
        }
    }

    return filteredLectures;
}




function outputTable(table, ID) {
    document.getElementById(ID).innerHTML = ''; 
    document.getElementById(ID).appendChild(table);
}


function makeTable() {
    var table = document.createElement('table');
    setTableStyleAndProperties(table);
    return table;
}


function addTableHead(table, heads) {
    var thead = table.createTHead();
    var row = thead.insertRow(0);

    for (var i = 0; i < heads.length; i++) {
	createHeaderCell(row, heads[i], 1, 1); 
    }
}


function addTableBody(table, items, makeCellFns) {
    var tbody = document.createElement('tbody');
    table.appendChild(tbody);

    for (var i = 0; i < items.length; i++) {
        var row  = tbody.insertRow(-1);
	
	for (var j = 0; j < makeCellFns.length; j++) {
	    makeCellFns[j](row, items[i]);
	}
    }   
}



function createHeaderCell(row, html, rowspan, colspan) {
    // creates header cell
    // text is a string, containing the text for the header. Can be HTML, for example 'Length <br> (mm:ss)'
    
    var cell = document.createElement('th');
    var textElement = createElementFromHTML(html);    

    cell.rowSpan = rowspan;
    cell.colSpan = colspan;
        
    cell.appendChild(textElement);
    cell.align = "center";

    row.appendChild(cell);
}


function createGenericCell(row, html) {
    //  creates a generic cell whose contents are set to the html string. 

    var cell = row.insertCell(-1);
    cell.align = "center";
    cell.appendChild(createElementFromHTML(html));
}

function createElementFromHTML(html){
    var textElement = document.createElement("div");
    textElement.innerHTML = html;
    return textElement;
}




//****************


// function createDownloadsCell(row, lecture){
//     var cell = row.insertCell(-1);
//     cell.align = "center";

//     var div = document.createElement('div');

//     div.className="course-lecture-item-resource";

    
//     if (lecture.notes)     { div.appendChild(createNotesLink(lecture)); }
//                              div.appendChild(createVideoDownloadLink(lecture));
//     if (lecture.inVideoQs) { div.appendChild(createQuizzesDownloadLink(lecture)); }
//                              div.appendChild(createTranscriptLink(lecture));
//                              div.appendChild(createSRTLink(lecture));

//     cell.appendChild(div);
// }

// function createNotesLink          (lecture){ return createDownloadLink(lecture, "icon-align-justify",         "Notes",             "rkt", createNotesURL(lecture)); }
// function createVideoDownloadLink  (lecture){ return createDownloadLink(lecture, "icon-download-alt resource", "Video",             "mp4", createVideoMP4URL(lecture)); }
// function createQuizzesDownloadLink(lecture){ return createDownloadLink(lecture, "icon-file",                  "In Video Quizzes",  "pdf", createQuizzesURL(lecture)); }
// function createTranscriptLink     (lecture){ return createDownloadLink(lecture, "icon-align-justify",         "Transcript",        "txt", createTranscriptURL(lecture)); }
// function createSRTLink            (lecture){ return createDownloadLink(lecture, "icon-list",                  "Subtitles",         "srt", createSRTURL(lecture)); }

// function createDownloadLink(lecture, icon, title, format, url){    

//     var link = document.createElement('a');
//     link.target = "_new";
//     link.href  = url;
//     link.title = title + " (" + format + ")";
//     link.appendChild(createElementFromHTML('<i class="' + icon + '" ></i>'));
//     link.appendChild(createElementFromHTML('<div class="hidden">' + link.title + '</div>'));

//     return link;
// }	



