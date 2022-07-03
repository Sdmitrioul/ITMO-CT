while (<>) {
    print if /.*\b(\d+(\.)?\d*)\b.*/;
}
