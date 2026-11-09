module FortQCDML
  use FortQCDML_base, only : qcdml_config, real64
  use FortQCDML_toml, only : read_qcdml_toml
  use FortQCDML_write, only : build_qcdml_xml
  implicit none(type, external)
  private
  public:: qcdml_config, real64
  public :: read_qcdml_toml
  public :: build_qcdml_xml
end module FortQCDML
