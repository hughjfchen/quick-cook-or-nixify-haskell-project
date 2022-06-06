import { fetchUtils } from "react-admin";
import postgrestDataProvider from '@raphiniert/ra-data-postgrest';

const servicesHost = 'http://www.detachmentsoft.top';
const servicesHostHome = 'http://www.detachmentsoft.top/rest';

const httpClient = (url, options = {}) => {
    if (!options.headers) {
        options.headers = new Headers({ Accept: 'application/json' });
    }
    const token = localStorage.getItem('token');
    options.headers.set('Authorization', `Bearer ${token}`);
    return fetchUtils.fetchJson(url, options);
};

const dataProvider = postgrestDataProvider(servicesHostHome, httpClient);

const myDataProvider = {
    ...dataProvider,
    create: (resource, params) => {
        if ( params.data.createReq !== "JobCreateReq") {
            // fallback to the default implementation
            return dataProvider.create(resource, params);
        }

        let formData = new FormData();

        formData.append('parsetype', params.data.parsetype);
        formData.append('file', params.data.file.rawFile);

        return httpClient(`${servicesHost}/parsedump`, {
            method: 'POST',
            body: formData,
        }).then(({ json }) => ({
            data: { ...params.data, id: json.id },
        }));
    }
};

export default myDataProvider;
