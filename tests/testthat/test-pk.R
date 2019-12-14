context("term finding and info APIs")

test_that("Test term details", {
  skip_on_cran()
  b <- pk_phenotype_detail("shape")
  c <- pk_anatomical_detail("basihyal bone")

  g <- pk_gene_detail("socs5")
  gg <- pk_gene_detail("socs5", "Danio rerio")

  expect_is(b, 'data.frame')
  expect_equal(nrow(b), 1)
  expect_false(any(is.na(b)))
  expect_is(c, 'data.frame')
  expect_equal(nrow(c), 1)
  expect_false(any(is.na(c)))
  

  expect_warning(bb <- pk_phenotype_detail("shape tt"))
  expect_warning(cc <- pk_anatomical_detail("fin tt"))
  expect_true(is.na(bb))
  expect_true(is.na(cc))

  expect_is(g, "data.frame")
  expect_gt(length(unique(g$taxon.label)), 2)
  expect_setequal(unique(g$matchType), c("exact", "partial"))
  expect_is(gg, "data.frame")
  expect_length(unique(gg$taxon.label), 1)
  expect_length(unique(gg$label), 2)
})

test_that("taxon details works", {
  skip_on_cran()
  a <- pk_taxon_detail("Coralliozetus")

  expect_is(a, 'data.frame')
  expect_gt(nrow(a), 0)
  expect_setequal(colnames(a),
                  c("id", "label", "extinct", "rank.id", "rank.label", "common_name"))

  expect_warning(aa <- pk_taxon_detail("coral tt"))
  expect_warning(bb <- pk_taxon_detail("http://foobar.com/taxon"))
  expect_warning(cc <- pk_taxon_detail(c("coral tt", "Danio rerio")))
  expect_true(is.na(aa))
  expect_true(is.na(bb))
  expect_true(any(is.na(cc)))
  expect_false(all(is.na(cc)))
  expect_equal(nrow(cc), 2)
  expect_true(all(is.na(cc[1,])))

  dd <- pk_taxon_detail(c("Danio", "Danio rerio"))
  expect_equal(nrow(dd), 2)
  expect_true(any(is.na(dd$common_name)))
  expect_false(all(is.na(dd$common_name)))
})

test_that("is_extinct works", {
  skip_on_cran()

  ext <- pk_is_extinct("Fisherichthys")
  expect_is(ext, "logical")
  expect_equal(names(ext), c("Fisherichthys"))
  # make one an unresolvable typo
  expect_warning(ext <- pk_is_extinct(c("Fisheririchthys", "Tiktaalik")))
  expect_is(ext, "logical")
  expect_length(ext, 2)
  expect_true(any(is.na(ext)))
  expect_false(all(is.na(ext)))
  expect_equal(names(ext), c("Fisheririchthys", "Tiktaalik"))
})

test_that("Test retrieving IRI", {
  skip_on_cran()

  i <- get_term_iri("Coralliozetus", "vto")
  expect_equal(i, "http://purl.obolibrary.org/obo/VTO_0042955")
  expect_warning(ii <- get_term_iri("Coralliozetus TT", "vto"))
  expect_warning(iii <- get_term_iri("Coralliozetus", "pato"))
  expect_true(is.na(ii))
  expect_true(is.na(iii))

  tiris <- find_term("pelvic fin", definedBy = NA, matchTypes = c("exact"))
  expect_gt(nrow(tiris), 1)
  expect_silent(tiri <- get_term_iri("pelvic fin", as = NA, exactOnly = TRUE))
  expect_equal(tiri, "http://purl.obolibrary.org/obo/UBERON_0000152")

  tiris <- find_term("part_of", definedBy = NA)
  expect_true("isDefinedBy" %in% colnames(tiris))
  expect_true(all(is.na(tiris$isDefinedBy)))

  expect_warning(tiri <- get_term_iri("anatomical structure", as = NA, exactOnly = TRUE))
  expect_is(tiri, "character")
  expect_true(startsWith(tiri, "http://purl.obolibrary.org/obo/"))
})

test_that("Deprecated function forr retrieving IRI", {
  skip_on_cran()

  expect_warning(tt <- pk_get_iri("Coralliozetus", "vto"))
  expect_equal(tt, "http://purl.obolibrary.org/obo/VTO_0042955")
  expect_equal(tt, get_term_iri("Coralliozetus", "vto"))

  expect_warning(tt <- pk_get_iri("pelvic fin", as = NA, exactOnly = TRUE))
  expect_equal(tt, "http://purl.obolibrary.org/obo/UBERON_0000152")
  expect_equal(tt, get_term_iri("pelvic fin", as = NA, exactOnly = TRUE))
})

test_that("Test getting labels", {
  tt <- c("http://purl.obolibrary.org/obo/UBERON_0000981",
          "http://purl.obolibrary.org/obo/UBERON_0002103",
          "http://purl.obolibrary.org/obo/UBERON_0000976",
          "http://purl.obolibrary.org/obo/UBERON_0002102")
  lbls <- get_term_label(tt)
  testthat::expect_equal(length(tt), nrow(lbls))
  testthat::expect_false(any(is.na(lbls$label)))
  testthat::expect_setequal(tt, lbls$id)

  lbls <- get_term_label(tt, preserveOrder = TRUE)
  testthat::expect_equal(tt, lbls$id)

  testthat::expect_silent(lbls <- get_term_label(c(tt, "http://foo")))
  testthat::expect_equal(sum(is.na(lbls$label)), 1)
  testthat::expect_equal(lbls$id[is.na(lbls$label)], "http://foo")

  lbls <- get_term_label(tt[1])
  testthat::expect_equal(nrow(lbls), 1)
  testthat::expect_false(is.na(lbls$label))

  lbls <- get_term_label("urn:foobar")
  testthat::expect_equal(nrow(lbls), 1)
  testthat::expect_equal(lbls$id, "urn:foobar")
  testthat::expect_true(is.na(lbls$label))

})
