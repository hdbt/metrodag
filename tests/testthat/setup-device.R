# Measuring grobs (for example in ggplotGrob()) opens R's default device when
# none is open, which would write Rplots.pdf beside the tests. Use a PDF
# device without a file instead.
options(device = function(...) grDevices::pdf(file = NULL, ...))
