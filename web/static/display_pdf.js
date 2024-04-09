/* Loaded by /datum to display a PDF in pdf.js */

function $(id) { return document.getElementById(id); }

function fetch_and_display_pdf_preview() {
	const url = $("raw-url").getAttribute("href");
	const { pdfjsLib } = globalThis;
	/* FIXME: BAD!  Figure out static assets policy then look for
	 * this locally! */
	pdfjsLib.GlobalWorkerOptions.workerSrc = 'https://mozilla.github.io/pdf.js/build/pdf.worker.mjs';
	/* Pull the PDF, painting just the first page onto the canvas
	 * FIXME: Figure out how to actually display the full PDF in
	 * the viewer, this hello-world script from the pdfjs docs
	 * really doesn't cut it :/ */
	const loadingTask = pdfjsLib.getDocument(url);
	loadingTask.promise.then(function(pdf) {
		pdf.getPage(1).then(function(page) {
			const canvas   = $("pdfjs-canvas");
			const scale    = 1.5;
			const viewport = page.getViewport({scale: scale});
			const context  = canvas.getContext("2d");
			canvas.height  = viewport.height;
			canvas.width   = viewport.width;
			const renderContext = {
				canvasContext: context,
				viewport: viewport
			};
			const renderTask = page.render(renderContext);
		});
		
	}, function (reason) {
		console.error("PDF loading failed!")
		console.error(reason);
	});
}

$("pdfjs-script").addEventListener("load", fetch_and_display_pdf_preview);
