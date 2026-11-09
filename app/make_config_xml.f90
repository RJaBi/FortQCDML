program make_config_xml
   use FortQCDML, only : &
      read_qcdml_toml, &
      build_qcdml_xml, &
      qcdml_config
   use foxy, only : xml_file
   implicit none(type, external)

   type(qcdml_config) :: config
   type(xml_file)     :: document

   call read_qcdml_toml("example/t2.toml", config)
   call build_qcdml_xml(config, document)

   call document%save( &
      filename="qcdml-config.xml", &
      is_declared=.true. &
   )
end program make_config_xml

