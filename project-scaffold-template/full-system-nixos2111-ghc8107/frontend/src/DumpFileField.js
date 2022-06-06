import * as React from 'react';
import { memo, FC, ElementType } from 'react';
import get from 'lodash/get';
import Typography, { TypographyProps } from '@material-ui/core/Typography';
import { useRecordContext } from 'ra-core';

import { sanitizeFieldRestProps, PublicFieldProps, InjectedFieldProps, fieldPropTypes } from 'react-admin';

const DumpFileField: FC<DumpFileFieldProps> = memo(props => {
    const { className, source, emptyText, ...rest } = props;
    const record = useRecordContext(props);
    const value = get(record, source);

    return (
        <Typography
            component="span"
            variant="body2"
            className={className}
            {...sanitizeFieldRestProps(rest)}
        >
            {value != null && typeof value !== 'string'
                ? JSON.stringify(value)
                : value || emptyText}
        </Typography>
    );
});

// what? TypeScript loses the displayName if we don't set it explicitly
DumpFileField.displayName = 'DumpFileField';

DumpFileField.defaultProps = {
    addLabel: true,
};

DumpFileField.propTypes = {
    // @ts-ignore
    ...Typography.propTypes,
    ...fieldPropTypes,
};

export interface DumpFileFieldProps
    extends PublicFieldProps,
        InjectedFieldProps,
        TypographyProps {
    // TypographyProps do not expose the component props, see https://github.com/mui-org/material-ui/issues/19512
    component?: ElementType<any>;
}

export default DumpFileField;
