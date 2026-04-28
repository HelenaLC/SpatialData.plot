# TODO: deprecate this; all should be handled by SD

# count occurrences of each coordinate space;
# return most frequent (in order of appearance)
.guess_space <- \(x) {
    nms <- lapply(rownames(x), \(l) 
        lapply(colnames(x)[[l]], \(e) 
            CTname(x[[l]][[e]])))
    cnt <- table(nms <- unlist(nms))
    cnt <- cnt[unique(nms)]
    names(which.max(cnt))
}

.trans_xy <- \(xy, ts, rev=FALSE) {
    if (rev) ts <- rev(ts)
    for (. in seq_along(ts)) {
        t <- ts[[.]]$type
        d <- ts[[.]]$data
        d <- unlist(d)
        if (length(d) == 3)
            d <- d[-1]
        switch(t, 
            identity={},
            scale={
                op <- ifelse(rev, `/`, `*`)
                xy$x <- op(xy$x, d[2])
                xy$y <- op(xy$y, d[1])
            },
            rotate={
                xy <- xy %*% .R(d*pi/180)
            },
            translation={
                op <- ifelse(rev, `-`, `+`)
                xy$x <- op(xy$x, d[2])
                xy$y <- op(xy$y, d[1])
            })
    }
    return(xy)
}
