import * as React from 'react';
import { AnchorHTMLAttributes, memo, FC } from 'react';
import get from 'lodash/get';
import { Typography, Link } from '@material-ui/core';
import { useRecordContext } from 'ra-core';
import { sanitizeFieldRestProps, PublicFieldProps, InjectedFieldProps, fieldPropTypes } from 'react-admin';
import { makeStyles } from '@material-ui/core/styles';
import LaunchIcon from '@material-ui/icons/Launch';

const useStyles = makeStyles({
    link: {
        textDecoration: 'none',
    },
    icon: {
        width: '0.5em',
        height: '0.5em',
        paddingLeft: 2,
    },
});

const UrlFieldWithLastFileNameAsLinkText: FC<UrlFieldWithLastFileNameAsLinkTextProps> = memo(props => {
    const { className, emptyText, source, linkText, ...rest } = props;
    const record = useRecordContext(props);
    const value = get(record, source);
    const classes = useStyles();

    if (value == null) {
        return (
            <Typography
                component="span"
                variant="body2"
                className={className}
                {...sanitizeFieldRestProps(rest)}
            >
                {emptyText}
            </Typography>
        );
    }

    return (
        <Link
          className={classes.link}
            href={value}
            variant="body2"
            {...sanitizeFieldRestProps(rest)}
        >
          {value.split('/').pop()}
          <LaunchIcon className={classes.icon} />
        </Link>
    );
});

UrlFieldWithLastFileNameAsLinkText.defaultProps = {
    addLabel: true,
};

UrlFieldWithLastFileNameAsLinkText.propTypes = fieldPropTypes;
UrlFieldWithLastFileNameAsLinkText.displayName = 'UrlFieldWithLastFileNameAsLinkText';

export interface UrlFieldWithLastFileNameAsLinkTextProps
    extends PublicFieldProps,
        InjectedFieldProps,
        AnchorHTMLAttributes<HTMLAnchorElement> {}

export default UrlFieldWithLastFileNameAsLinkText;
