import React from 'react';
import { Admin, Resource } from 'react-admin';
import myDataProvider from './dataprovider';
import myAuthProvider from './auth';
import { JobList, JobCreate } from './jobs';


const App = () => (
    <Admin disableTelemetry title="Java Dump Analyzer" dataProvider={myDataProvider} authProvider={myAuthProvider}>
        <Resource name="jobs" list={JobList} create={JobCreate}/>
    </Admin>
);

export default App;
