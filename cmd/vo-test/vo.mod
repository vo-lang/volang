module github.com/vo-lang/volang/cmd/vo-test

vo 0.1

require github.com/vo-lang/vox v0.1.0

replace github.com/vo-lang/vox => ../../../vox

files (
    main.vo
    config.vo
    files.vo
    runner.vo
)
