

val migrationFileFormat = """^([0-9])+_[a-zA-Z0-9_-]+\.dh"""

migrationFileFormat.r.findFirstMatchIn("1_haha.dh") map ( _.group(0) )