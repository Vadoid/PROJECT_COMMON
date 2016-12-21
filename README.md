# BADI developments for BPC

#Common Class (ZCL_BPC_COMMON)

**VERSION CONTROL**

**v.001 VZ - December 2016**

**INFORMATION ABOUT CLASS**
 This class consists of various methods which we find useful in BPC
 BADI developments. This class could be called from BADI and parameters
 could be passed to the required methods. This Class id Generic in it's
 nature and could be reused on different implementations with the
 relevant adjustments.

 Current list of methods in ZBPC_COMMON_CLASS:
 1. TEST_AND_DEBUG - service method with basic information and place
    for testing and debugging. This class is not required and could
    be delited on customer implementation after the initial unit testing.

 2. READ_MODEL_DATA - reads model data and returns the read values back.
    Accepts IT_SEL and IT_CV as export parameters to filter data.
    Environment and Model IDs to be read have to be passed. The data is
    imported as OUTPUT_DATA Standard Table and has to be assigned to the
    field symbol or internal table of same type.

 3. WRITE_MODEL_DATA - writes data back to the model. Could be used for
    cross-model writing. INPUT_DATA is the exporting parameter along with
    Environment, Model and Target model. If the Target Model is empty,
    data is written into the same model under exporting parameter.
    Method should be checking work status and relevant security, returning
    ET_MESSAGE and ET_ERROR_RECORDS if something went wrong. This will have
    to be passed to the calling BADI.

 4. READ_MASTER_DATA - read master data of the relevant dimension, pass back.
    it_sel, Environment, Model and Dimension are the Importing Params, which
    means that the filter could be passed into method.

 5. READ_MASTER_DATA_CHILDREN - children of the particular hierarchy node,
    passed in the Importing parameter. Returns all children and can later
    be reused in READ_MODEL_DATA filter.
	
**FAIR USE**

Please feel free to reuse this class in your implementations. Just drop me a quick line 
about your project.

