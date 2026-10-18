jdemetra_selector <- init_selector(
    source = "GitHub",
    owner = "jdemetra",
    repo = "jdplus-main",
    state = "all"
)

update_database(
    selector = jdemetra_selector,
    dataset_dir = file.path("inst", "data_issues")
)
