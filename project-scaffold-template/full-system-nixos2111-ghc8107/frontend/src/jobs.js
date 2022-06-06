// in jobs.js
import * as React from "react";
import {
    List,
    Datagrid,
    TextField,
    DateField,
    FileField,
    Create,
    SimpleForm,
    RadioButtonGroupInput,
  FileInput,
  required,
  choices,
} from 'react-admin';
import UrlFieldWithCustomLinkText from './UrlFieldWithCustomLinkText';
import UrlFieldWithLastFileNameAsLinkText from './UrlFieldWithLastFileNameAsLinkText';

         // <FunctionField render={record => `${record.payload.contents.contents.split('/').pop()}`} label="Dump File"/>
         // <TextField source="payload.contents.contents" label="Dump File"/>
         // <DumpFileField source="payload.contents.contents" label="Dump File"/>
export const JobList = props => (
    <List {...props}>
       <Datagrid>
         <TextField source="payload.tag" label="Parse Type"/>
         <UrlFieldWithLastFileNameAsLinkText source="payload.contents.contents" download={true} label="Dump File"/>
         <DateField source="created_at" showTime={true} locales='zh-CN' options={{hour12: false}}/>
         <DateField source="run_at" showTime={true} locales='zh-CN' options={{hour12: false}}/>
         <DateField source="updated_at" showTime={true} locales='zh-CN' options={{hour12: false}}/>
         <TextField source="status" />
         <UrlFieldWithCustomLinkText source="last_update.report_url" linkText="Report" target="_blank" label="Report"/>
       </Datagrid>
   </List>
);

const validateParseType = [required(), choices(['ParseJavaCore', 'ParseHeapDump'], 'Please choose one of the values')];
const validateDumpFile = [required()]
const tagJobCreateReq = data => ({
  ...data,
  createReq: "JobCreateReq"
});

export const JobCreate = props => (
     <Create {...props} transform={tagJobCreateReq} >
        <SimpleForm>
          <RadioButtonGroupInput source="parsetype" choices={[
            { id: 'ParseJavaCore', name: 'Java Core' },
            { id: 'ParseHeapDump', name: 'Heap Dump' },
          ]} validate={validateParseType}/>
          <FileInput source="file" label="Java Dump File" multiple={false} minSize={0} maxSize={1000000000} placeholder={<p>Drop your java dump file here</p>} validate={validateDumpFile}>
            <FileField source="src" title="title" />
          </FileInput>
        </SimpleForm>
     </Create>
);
